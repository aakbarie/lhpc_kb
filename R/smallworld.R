# Small-world structure for the policy graph.
#
# WHY THIS MATTERS FOR RETRIEVAL, not just as a network statistic.
#
# Graph-augmented RAG works by seeding from vector search and then walking
# the graph to gather context the seed alone does not carry. How well that
# walk works is a property of the graph's topology, and two failure modes
# bracket it:
#
#   * All clusters, no shortcuts (high C, high L). Every plan is a silo
#     around its own plan node, so a walk from a Kern policy never reaches
#     Partnership's equivalent within a usable hop budget. Retrieval returns
#     one plan's view and reads as if it were the whole picture.
#   * All shortcuts, no clusters (low C, low L). Neighbours are arbitrary, so
#     expansion adds noise rather than context.
#
# Small-world is the regime that serves retrieval: neighbourhoods stay
# coherent (a policy's neighbours really are about the same thing) while any
# two documents sit a few hops apart.
#
# The intervention is deliberately small. Watts and Strogatz showed that
# rewiring a ~1% fraction collapses path length while leaving clustering
# almost untouched, and ../Topological_Origin_of_Small-World_Networks derives
# why that fraction is what it is: p_eff = 1 - Λ/B, with the small-world
# window at Λ/B ≈ 0.01–0.05 and breakdown above 0.1. That paper also predicts
# our exact situation — "highly modular networks with incompatible
# constraints should exhibit long paths even after extensive rewiring", and
# "adding compatible coordinates should lower Λ/B and move the system into
# the small-world phase".
#
# So this file measures the graph honestly (C, L, σ, ω — standard
# definitions, no proxies), then adds a small budget of *compatible*
# shortcuts: cross-plan pairs that are semantically close but topologically
# far. Random rewiring would also shorten paths, and would also destroy the
# graph's meaning; every shortcut added here is one a domain expert would
# recognise as a real correspondence.
#
# Usage:
#   Rscript -e 'source("R/smallworld.R"); print(sw_metrics())'
#   Rscript -e 'source("R/smallworld.R"); sw_augment(p = 0.02)'
#   Rscript -e 'source("R/smallworld.R"); print(sw_report())'

suppressPackageStartupMessages({
  library(igraph); library(DBI); library(duckdb); library(jsonlite)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "config.R", "graph.R"))
    source(file.path(d, "R", f))
})

# Edge types that carry *meaning* between documents. published_by and
# owned_by are excluded on purpose: they are stars through a plan or
# department hub, and a hub makes every path look short while connecting
# nothing a reader would call related. Including them would report a
# small-world graph that does not help retrieval at all.
SEMANTIC_EDGES <- c("implements", "supersedes", "similar_topic", "shortcut")

#' The retrieval-relevant subgraph as an undirected igraph.
sw_graph <- function(cfg = kb_config(), types = SEMANTIC_EDGES) {
  con <- dbConnect(duckdb(), graph_path(cfg), read_only = TRUE, array = "matrix")
  on.exit(dbDisconnect(con, shutdown = TRUE))
  el <- dbGetQuery(con, sprintf(
    "SELECT source, target FROM edges WHERE relationship_type IN ('%s')",
    paste(types, collapse = "','")))
  if (!nrow(el)) stop("no semantic edges in the graph — run build_graph() first")
  simplify(graph_from_data_frame(el, directed = FALSE))
}

#' Watts–Strogatz metrics, with the two reference graphs the definitions need.
#'
#' L is averaged over connected pairs only. That is the honest choice for a
#' disconnected graph — the alternative, treating unreachable pairs as
#' infinite, makes L meaningless — but it also means L alone understates the
#' problem, so `reach` (the fraction of pairs that are connected at all) is
#' reported beside it and is the number that actually matters for retrieval.
sw_metrics <- function(g = sw_graph(), n_rand = 5, seed = 42) {
  set.seed(seed)
  n <- vcount(g); m <- ecount(g)
  comp <- components(g)

  C <- transitivity(g, type = "global")
  L <- mean_distance(g, directed = FALSE, unconnected = TRUE)
  reach <- sum(comp$csize * (comp$csize - 1)) / (n * (n - 1))

  # Degree-preserving randomisation is the right null model: it holds the
  # degree sequence fixed, so any change in C or L is topology, not density.
  rand <- replicate(n_rand, {
    r <- rewire(g, keeping_degseq(niter = 10 * m))
    c(C = transitivity(r, type = "global"),
      L = mean_distance(r, directed = FALSE, unconnected = TRUE))
  })
  C_rand <- mean(rand["C", ], na.rm = TRUE)
  L_rand <- mean(rand["L", ], na.rm = TRUE)

  # Lattice reference for omega. A ring lattice at the observed mean degree
  # is the standard high-clustering comparison.
  # nei must be >= 2 or the "lattice" is a bare ring with no triangles at
  # all, C_latt = 0, and omega comes back -Inf rather than undefined.
  k <- max(2, round(mean(degree(g))))
  latt <- sample_smallworld(1, n, max(2, floor(k / 2)), 0)
  C_latt <- transitivity(latt, type = "global")
  omega <- if (is.finite(C_latt) && C_latt > 0) (L_rand / L) - (C / C_latt) else NA_real_

  list(
    nodes = n, edges = m,
    components = comp$no, largest_component = max(comp$csize),
    reach = reach,
    C = C, L = L, C_rand = C_rand, L_rand = L_rand, C_latt = C_latt,
    sigma = (C / C_rand) / (L / L_rand),
    omega = omega
  )
}

#' Document centroid vectors from the chunk store: a document is the mean of
#' its chunk embeddings. Returns NULL when the store is absent or unbuilt, so
#' callers can fall back rather than fail.
sw_doc_vectors <- function(cfg = kb_config(), min_chunks = 1) {
  if (!file.exists(cfg$store_path)) return(NULL)
  con <- tryCatch(dbConnect(duckdb(), cfg$store_path, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con, shutdown = TRUE))
  ok <- tryCatch(dbExistsTable(con, "embeddings"), error = function(e) FALSE)
  if (!ok) return(NULL)

  df <- tryCatch(dbGetQuery(con, sprintf("
      SELECT plan_id, filename, embedding FROM chunks
      WHERE plan_id <> '' GROUP BY ALL HAVING count(*) >= %d", min_chunks)),
    error = function(e) NULL)
  if (is.null(df) || !nrow(df)) {
    df <- tryCatch(dbGetQuery(con, "SELECT plan_id, filename, embedding FROM chunks"),
                   error = function(e) NULL)
    if (is.null(df) || !nrow(df)) return(NULL)
  }
  emb <- df$embedding
  if (!is.matrix(emb)) emb <- do.call(rbind, emb)
  key <- paste0("doc:", file.path(df$plan_id, df$filename))
  cent <- rowsum(emb, key) / as.vector(table(key)[unique(key)][rownames(rowsum(emb, key))])
  cent <- cent / sqrt(rowSums(cent^2))            # unit-normalise for cosine
  cent[is.finite(rowSums(cent)), , drop = FALSE]
}

#' Candidate shortcuts: cross-plan document pairs that are semantically close
#' but topologically far.
#'
#' Both halves of that are load-bearing. Semantic closeness is what keeps a
#' shortcut meaningful — this is the "compatible coordinate" the topological
#' account calls for, not a random rewire. Topological distance is what makes
#' it a *shortcut*: linking two documents that are already two hops apart
#' costs an edge and buys nothing.
sw_candidates <- function(g, cfg = kb_config(), per_doc = 3, min_sim = 0.55,
                          min_hops = 4) {
  V <- sw_doc_vectors(cfg)
  if (is.null(V) || nrow(V) < 2) {
    message("  no usable embeddings yet — falling back to title-term candidates")
    return(sw_candidates_terms(g, cfg, min_hops = min_hops))
  }
  plan_of <- sub("^doc:([^/]+)/.*$", "\\1", rownames(V))

  sims <- V %*% t(V)
  diag(sims) <- -Inf
  out <- list()
  for (i in seq_len(nrow(V))) {
    ord <- order(sims[i, ], decreasing = TRUE)
    taken <- 0L
    for (j in ord) {
      if (taken >= per_doc || sims[i, j] < min_sim) break
      if (plan_of[i] == plan_of[j]) next                       # cross-plan only
      a <- rownames(V)[i]; b <- rownames(V)[j]
      if (a >= b) next                                          # one per pair
      out[[length(out) + 1]] <- data.frame(
        source = a, target = b, sim = sims[i, j], stringsAsFactors = FALSE)
      taken <- taken + 1L
    }
  }
  if (!length(out)) return(NULL)
  cand <- do.call(rbind, out)
  sw_filter_by_distance(cand, g, min_hops)
}

#' Title-term fallback, used before the chunk store exists.
sw_candidates_terms <- function(g, cfg = kb_config(), min_hops = 4) {
  cat <- read_catalog()
  e <- similar_topic_edges(cat, per_node = 10, min_shared = 3)
  if (is.null(e)) return(NULL)
  cand <- data.frame(source = e$source, target = e$target,
                     sim = e$weight / max(e$weight), stringsAsFactors = FALSE)
  sw_filter_by_distance(cand, g, min_hops)
}

#' Keep only pairs that are genuinely far apart in the current graph
#' (including pairs in different components, which are infinitely far and are
#' the highest-value shortcuts of all).
sw_filter_by_distance <- function(cand, g, min_hops) {
  known <- V(g)$name
  cand$in_graph <- cand$source %in% known & cand$target %in% known
  cand$hops <- Inf
  idx <- which(cand$in_graph)
  if (length(idx)) {
    for (s in unique(cand$source[idx])) {
      rows <- idx[cand$source[idx] == s]
      d <- distances(g, v = s, to = cand$target[rows])
      cand$hops[rows] <- as.vector(d)
    }
  }
  keep <- !is.finite(cand$hops) | cand$hops >= min_hops
  cand <- cand[keep, , drop = FALSE]
  cand[order(-cand$sim), , drop = FALSE]
}

#' Add a budget of shortcut edges and write them into the graph.
#'
#' `p` is the rewiring fraction, expressed against the semantic edge count —
#' the same quantity as Watts–Strogatz p, and comparable to the p_eff the
#' topological account predicts. Default 0.02 sits inside the 0.01–0.05
#' window; the point of the exercise is that a small budget is enough.
sw_augment <- function(cfg = kb_config(), p = 0.02, min_sim = 0.55,
                       min_hops = 4, dry_run = FALSE) {
  g <- sw_graph(cfg)
  before <- sw_metrics(g)
  message(sprintf("semantic graph: %d nodes, %d edges | reach %.3f, sigma %.2f",
                  before$nodes, before$edges, before$reach, before$sigma))

  # A disconnected graph cannot be moved into the small-world regime by
  # rewiring alone: bridging c components costs at least c-1 edges, and if
  # that floor already exceeds the small-world window then fragmentation,
  # not path length, is the binding constraint. Reporting it makes the
  # difference visible instead of hiding it behind an unreachable target.
  bridge_p <- (before$components - 1) / before$edges
  message(sprintf("connecting %d components needs >= %d edges (p = %.3f)%s",
                  before$components, before$components - 1, bridge_p,
                  if (bridge_p > 0.1) "  <- above the 0.1 breakdown threshold"
                  else if (bridge_p > 0.05) "  <- above the 0.01-0.05 window"
                  else ""))

  if (identical(p, "auto")) {
    p <- min(0.25, bridge_p * 1.3)
    message(sprintf("auto budget: p = %.3f (bridging cost + 30%% headroom)", p))
  }
  budget <- max(1L, round(p * ecount(g)))
  message(sprintf("shortcut budget: %d edges (p = %.3f)", budget, p))

  cand <- sw_candidates(g, cfg, min_sim = min_sim, min_hops = min_hops)
  if (is.null(cand) || !nrow(cand)) { message("no candidate shortcuts found"); return(invisible(NULL)) }

  # Prefer pairs that are currently unreachable, then the most similar.
  cand <- cand[order(is.finite(cand$hops), -cand$sim), , drop = FALSE]
  add <- head(cand, budget)
  message(sprintf("  %d candidates; taking %d (%d previously unreachable)",
                  nrow(cand), nrow(add), sum(!is.finite(add$hops))))
  if (dry_run) return(invisible(add))

  con <- dbConnect(duckdb(), graph_path(cfg), array = "matrix")
  on.exit(dbDisconnect(con, shutdown = TRUE))
  dbExecute(con, "DELETE FROM edges WHERE relationship_type = 'shortcut'")
  rows <- data.frame(
    source = add$source, target = add$target, relationship_type = "shortcut",
    weight = round(add$sim, 4), directed = FALSE,
    evidence = ifelse(is.finite(add$hops), paste0("was ", add$hops, " hops"),
                      "was unreachable"),
    stringsAsFactors = FALSE)
  dbAppendTable(con, "edges", rows)
  dbDisconnect(con, shutdown = TRUE); on.exit(NULL)

  after <- sw_metrics(sw_graph(cfg))
  message(sprintf("after: reach %.3f (was %.3f) | L %.2f (was %.2f) | C %.3f (was %.3f) | sigma %.2f (was %.2f)",
                  after$reach, before$reach, after$L, before$L,
                  after$C, before$C, after$sigma, before$sigma))
  invisible(list(added = rows, before = before, after = after))
}

#' Metrics as a readable frame, with the small-world verdict spelled out.
sw_report <- function(cfg = kb_config()) {
  m <- sw_metrics(sw_graph(cfg))
  verdict <- if (m$reach < 0.5) "fragmented — most document pairs cannot reach each other"
             else if (m$sigma > 1.5) "small-world"
             else if (m$sigma > 1) "weakly small-world"
             else "not small-world"
  data.frame(
    metric = c("nodes", "edges", "components", "reachable pairs",
               "clustering C", "path length L", "C/C_rand", "L/L_rand",
               "sigma", "omega", "verdict"),
    value = c(m$nodes, m$edges, m$components, sprintf("%.3f", m$reach),
              sprintf("%.3f", m$C), sprintf("%.2f", m$L),
              sprintf("%.2f", m$C / m$C_rand), sprintf("%.2f", m$L / m$L_rand),
              sprintf("%.2f", m$sigma), sprintf("%.2f", m$omega), verdict),
    stringsAsFactors = FALSE)
}
