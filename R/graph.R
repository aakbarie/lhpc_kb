# The policy graph: plans, documents, APLs and departments as one network.
#
# Why a graph and not just a search index. Retrieval answers "what does this
# corpus say about X". The questions that make an LHPC-wide knowledge base
# worth building are relational, and hybrid search cannot answer them:
#
#   * which plans have implemented APL 26-014, and which have not?
#   * this policy cites APL 22-030 — that APL was superseded twice since;
#     is the policy stale?
#   * show me every plan's version of the same requirement, side by side.
#
# Those are traversals. So the same catalog and chunk store that feed
# retrieval also feed a node/edge model, built into its own DuckDB file
# (data/lhpc_graph.duckdb) and exported as CSV.
#
# Edge-type vocabulary is deliberately the one already used in
# ../knowledge_base/app/network_analysis.py (supersedes / amends /
# references / similar_topic), extended with the relations a multi-plan
# corpus adds: published_by, owned_by, implements.
#
# Usage:
#   Rscript -e 'source("R/graph.R"); build_graph()'
#   Rscript -e 'source("R/graph.R"); build_graph(mine_text = FALSE)'  # fast

suppressPackageStartupMessages({
  library(jsonlite)
  library(DBI)
  library(duckdb)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "config.R")) source(file.path(d, "R", f))
})

graph_path <- function(cfg = kb_config())
  file.path(dirname(cfg$catalog), "lhpc_graph.duckdb")

# --- identifiers -------------------------------------------------------------

#' Canonical APL id: "APL 26-014", "apl26-014", "APL26 014" -> "26-014".
#' Anchored on the YY-NNN shape because that is what every plan's prose,
#' filenames and DHCS's own titles agree on.
#' A bare YY-NNN is NOT enough on its own — policy numbers of the same shape
#' are everywhere in this corpus ("Policy 600-1017"), so an id only counts
#' when an APL token introduces it. But DHCS writes lists —
#' "(Supersedes APLs 00-012, 18-022, and Policy Letters 98-006 …)" — where
#' only the first id carries the prefix. So: anchor on the APL token, then
#' consume the run of ids that follows while they are separated by nothing but
#' list punctuation. The run stops at "and Policy Letters", which is what
#' keeps 98-006 out.
APL_TOKEN   <- "\\d{2}\\s*[-‐-―_ ]\\s*\\d{3}"
APL_RUN_RE  <- paste0("^(?i)APLs?\\s*#?\\s*(", APL_TOKEN,
                      "(?:\\s*(?:,|and|&)\\s*", APL_TOKEN, ")*)")

apl_ids <- function(x) {
  lapply(x, function(s) {
    if (length(s) != 1 || is.na(s) || !nzchar(s)) return(character())
    # No trailing \b: the corpus contains "apl26-002" with no separator, and
    # l->2 is not a word boundary, so anchoring both ends misses it entirely.
    starts <- gregexpr("(?i)\\bAPLs?", s, perl = TRUE)[[1]]
    if (starts[1] == -1) return(character())
    out <- character()
    for (p in starts) {
      run <- regmatches(substr(s, p, nchar(s)),
                        regexpr(APL_RUN_RE, substr(s, p, nchar(s)), perl = TRUE))
      if (!length(run)) next
      ids <- regmatches(run, gregexpr(APL_TOKEN, run, perl = TRUE))[[1]]
      ids <- gsub("\\s", "", ids)
      out <- c(out, sub("^(\\d{2})[^0-9]?(\\d{3})$", "\\1-\\2", ids))
    }
    unique(out)
  })
}

node_id <- function(kind, key) paste0(kind, ":", key)

#' "<number> <title>", unless the title already opens with the number —
#' several corpora (DHCS's especially) bake the identifier into the title, and
#' prepending it unconditionally yields "APL 14-017: APL 14-017: ...".
label_with_number <- function(number, title) {
  number <- trimws(number %||% ""); title <- trimws(title %||% "")
  ifelse(!nzchar(number), title,
         ifelse(startsWith(tolower(title), tolower(number)), title,
                trimws(paste(number, title))))
}

# --- nodes -------------------------------------------------------------------

#' Plans, documents, departments and APLs as one node table.
#'
#' APL nodes are minted from every APL *mentioned* anywhere, not only those
#' with a PDF in data/pdf/dhcs_apl — a plan policy citing APL 19-001 should
#' still connect to something, and a dangling reference is itself a finding.
graph_nodes <- function(catalog, reg) {
  plans <- do.call(rbind, lapply(reg, function(p) data.frame(
    id = node_id("plan", p$plan_id), kind = "plan", plan_id = p$plan_id,
    label = p$name %||% p$plan_id, sublabel = p$domain %||% "",
    doc_kind = "", department = "", date_published = "",
    stringsAsFactors = FALSE)))

  docs <- data.frame(
    id = node_id("doc", file.path(catalog$plan_id, catalog$filename)),
    kind = "document", plan_id = catalog$plan_id,
    label = label_with_number(catalog$policy_number, catalog$title),
    sublabel = catalog$filename, doc_kind = catalog$doc_kind,
    department = catalog$department, date_published = catalog$date_published,
    stringsAsFactors = FALSE)

  depts <- unique(catalog[nzchar(catalog$department), c("plan_id", "department")])
  depts <- if (nrow(depts)) data.frame(
    id = node_id("dept", paste(depts$plan_id, depts$department, sep = ":")),
    kind = "department", plan_id = depts$plan_id, label = depts$department,
    sublabel = "", doc_kind = "", department = depts$department,
    date_published = "", stringsAsFactors = FALSE) else NULL

  list(plans = plans, docs = docs, depts = depts)
}

# --- edges -------------------------------------------------------------------

edge_row <- function(source, target, type, weight = 1, directed = TRUE,
                     evidence = "") {
  if (!length(source)) return(NULL)
  data.frame(source = source, target = target, relationship_type = type,
             weight = weight, directed = directed, evidence = evidence,
             stringsAsFactors = FALSE)
}

#' Structural edges: exact, free, and the backbone of every traversal.
structural_edges <- function(catalog) {
  doc <- node_id("doc", file.path(catalog$plan_id, catalog$filename))
  out <- list(
    edge_row(doc, node_id("plan", catalog$plan_id), "published_by")
  )
  has_dept <- nzchar(catalog$department)
  if (any(has_dept))
    out <- c(out, list(edge_row(
      doc[has_dept],
      node_id("dept", paste(catalog$plan_id[has_dept],
                            catalog$department[has_dept], sep = ":")),
      "owned_by")))
  do.call(rbind, out)
}

#' APL supersession, parsed from DHCS's own titles.
#'
#' Two directions appear in the corpus and both must be read, because they
#' describe the same edge from opposite ends:
#'   "APL 26-001: ... (Supersedes APL 22-030)"     -> 26-001 supersedes 22-030
#'   "APL 00-006: ... Superseded by APL 14-017"    -> 14-017 supersedes 00-006
#' Reading only the first would silently halve the chain and, worse, leave the
#' newest letter looking like a leaf.
supersede_edges <- function(catalog) {
  apls <- catalog[catalog$doc_kind == "apl", , drop = FALSE]
  if (!nrow(apls)) return(NULL)
  rows <- list()
  for (i in seq_len(nrow(apls))) {
    title <- apls$title[i]
    self <- apl_ids(apls$policy_number[i])[[1]]
    if (!length(self)) self <- apl_ids(title)[[1]][1]
    if (!length(self)) next
    self <- self[1]

    # Split on the phrase so each side's APL ids are attributed correctly.
    by_pos <- regexpr("(?i)supersede[ds]?\\s+by", title, perl = TRUE)
    sup_pos <- regexpr("(?i)supersedes", title, perl = TRUE)

    if (by_pos > 0) {
      tail <- substr(title, by_pos, nchar(title))
      for (t in setdiff(apl_ids(tail)[[1]], self))
        rows <- c(rows, list(edge_row(node_id("apl", t), node_id("apl", self),
                                      "supersedes", 3, TRUE, "title")))
    }
    if (sup_pos > 0 && (by_pos < 0 || sup_pos != by_pos)) {
      tail <- substr(title, sup_pos, if (by_pos > sup_pos) by_pos - 1 else nchar(title))
      for (t in setdiff(apl_ids(tail)[[1]], self))
        rows <- c(rows, list(edge_row(node_id("apl", self), node_id("apl", t),
                                      "supersedes", 3, TRUE, "title")))
    }
  }
  do.call(rbind, rows)
}

#' Every APL id a document's own text mentions -> `implements` edges.
#'
#' This is the cross-plan spine, and it has to come from text: exactly ONE
#' document in 2,447 names an APL in its title. Reads the chunk store, so it
#' needs ingest to have run; without it the graph is still built, just without
#' this edge type (and build_graph() says so rather than quietly shrinking).
implements_edges <- function(cfg) {
  if (!file.exists(cfg$store_path)) {
    message("  (no chunk store yet — skipping text-mined `implements` edges)")
    return(NULL)
  }
  con <- dbConnect(duckdb(), cfg$store_path, read_only = TRUE, array = "matrix")
  on.exit(dbDisconnect(con, shutdown = TRUE))

  # Pull only chunks that mention an APL at all; the regex work happens in R
  # but the scan is pushed down to DuckDB.
  rows <- dbGetQuery(con, "
    SELECT plan_id, filename, text
    FROM chunks
    WHERE plan_id <> 'dhcs_apl' AND regexp_matches(text, '(?i)APL\\s*#?\\s*[0-9]{2}[- ][0-9]{3}')")
  if (!nrow(rows)) return(NULL)

  ids <- apl_ids(rows$text)
  keep <- lengths(ids) > 0
  if (!any(keep)) return(NULL)

  src <- node_id("doc", file.path(rows$plan_id[keep], rows$filename[keep]))
  n <- lengths(ids[keep])
  out <- data.frame(
    source = rep(src, n),
    target = node_id("apl", unlist(ids[keep])),
    relationship_type = "implements", weight = 1, directed = TRUE,
    evidence = "text", stringsAsFactors = FALSE)
  # One edge per (document, APL) however many chunks mention it; weight
  # records how insistently the document refers to it.
  agg <- aggregate(list(weight = out$weight),
                   by = list(source = out$source, target = out$target), FUN = sum)
  agg$relationship_type <- "implements"; agg$directed <- TRUE; agg$evidence <- "text"
  agg[, c("source", "target", "relationship_type", "weight", "directed", "evidence")]
}

#' Documents on the same subject, linked by shared distinctive title terms.
#'
#' Deliberately cheap and deliberately capped: an inverted index over title
#' terms (as ../knowledge_base/app/network_analysis.py does for tags), pairs
#' scored by shared-term count, and at most `per_node` neighbours kept. The
#' cross-plan pairs are the interesting ones — they are what "show me every
#' plan's version of this requirement" walks — so same-plan pairs are dropped.
similar_topic_edges <- function(catalog, per_node = 6, min_shared = 2) {
  stop_terms <- c("policy", "policies", "procedure", "procedures", "provider",
                  "providers", "health", "plan", "plans", "medi", "cal",
                  "medical", "member", "members", "manual", "and", "for", "the",
                  "of", "to", "in", "a", "services", "service", "care", "apl",
                  "program", "management", "review", "requirements", "form")
  id <- node_id("doc", file.path(catalog$plan_id, catalog$filename))
  terms <- strsplit(tolower(gsub("[^a-z0-9 ]+", " ", tolower(catalog$title))), " +")
  terms <- lapply(terms, function(t) unique(t[nchar(t) > 3 & !t %in% stop_terms]))

  inv <- new.env(parent = emptyenv())
  for (i in seq_along(terms)) for (t in terms[[i]])
    assign(t, c(get0(t, inv, ifnotfound = integer()), i), inv)

  pair_count <- new.env(parent = emptyenv())
  for (t in ls(inv)) {
    idx <- get(t, inv)
    if (length(idx) < 2 || length(idx) > 60) next     # a term in 60 titles is not distinctive
    for (a in seq_along(idx)) for (b in seq_len(a - 1)) {
      i <- idx[a]; j <- idx[b]
      if (catalog$plan_id[i] == catalog$plan_id[j]) next      # cross-plan only
      k <- paste(min(i, j), max(i, j))
      assign(k, get0(k, pair_count, ifnotfound = 0L) + 1L, pair_count)
    }
  }
  keys <- ls(pair_count)
  if (!length(keys)) return(NULL)
  counts <- vapply(keys, function(k) get(k, pair_count), integer(1))
  keys <- keys[counts >= min_shared]; counts <- counts[counts >= min_shared]
  if (!length(keys)) return(NULL)

  ij <- do.call(rbind, lapply(strsplit(keys, " "), as.integer))
  df <- data.frame(i = ij[, 1], j = ij[, 2], w = as.integer(counts))
  df <- df[order(-df$w), ]
  # Cap per node so one popular subject cannot dominate the layout.
  deg <- integer(nrow(catalog))
  keep <- logical(nrow(df))
  for (r in seq_len(nrow(df))) {
    if (deg[df$i[r]] >= per_node || deg[df$j[r]] >= per_node) next
    keep[r] <- TRUE; deg[df$i[r]] <- deg[df$i[r]] + 1L; deg[df$j[r]] <- deg[df$j[r]] + 1L
  }
  df <- df[keep, , drop = FALSE]
  if (!nrow(df)) return(NULL)
  edge_row(id[df$i], id[df$j], "similar_topic", df$w, FALSE, "title terms")
}

# --- build -------------------------------------------------------------------

#' Build the graph into data/lhpc_graph.duckdb (+ CSV export).
#'
#' Built to a temp path and renamed into place, for the same reason ingest is:
#' a reader must never see a half-built graph, and a failed run must leave the
#' working one untouched.
build_graph <- function(cfg = kb_config(), mine_text = TRUE, csv = TRUE) {
  catalog <- read_catalog()
  if (nrow(catalog) == 0) stop("No catalog. Run fetch_all() first.")
  reg <- kb_registry()

  message("building graph over ", nrow(catalog), " documents / ",
          length(unique(catalog$plan_id)), " plans")

  n <- graph_nodes(catalog, reg)
  edges <- list(structural_edges(catalog), supersede_edges(catalog))
  if (mine_text) edges <- c(edges, list(implements_edges(cfg)))
  edges <- c(edges, list(similar_topic_edges(catalog)))
  edges <- do.call(rbind, Filter(Negate(is.null), edges))

  # Mint an APL node for every APL referenced by anything, so no edge dangles.
  apl_keys <- unique(sub("^apl:", "",
                         grep("^apl:", c(edges$source, edges$target), value = TRUE)))
  apl_nodes <- if (length(apl_keys)) {
    lab <- catalog$title[match(paste0("APL ", apl_keys), catalog$policy_number)]
    data.frame(id = node_id("apl", apl_keys), kind = "apl", plan_id = "dhcs_apl",
               label = ifelse(is.na(lab), paste("APL", apl_keys),
                              label_with_number(paste("APL", apl_keys), lab)),
               sublabel = paste("APL", apl_keys), doc_kind = "apl",
               department = "State Guidance", date_published = "",
               stringsAsFactors = FALSE)
  } else NULL

  nodes <- do.call(rbind, Filter(Negate(is.null),
                                 list(n$plans, n$docs, n$depts, apl_nodes)))
  nodes <- nodes[!duplicated(nodes$id), , drop = FALSE]

  # An edge to a node we never minted would break every traversal silently.
  known <- nodes$id
  dangling <- !(edges$source %in% known & edges$target %in% known)
  if (any(dangling)) {
    message("  dropping ", sum(dangling), " edge(s) with unknown endpoints")
    edges <- edges[!dangling, , drop = FALSE]
  }
  edges <- edges[!duplicated(edges[, c("source", "target", "relationship_type")]), ]

  path <- graph_path(cfg); target <- paste0(path, ".building")
  unlink(c(target, paste0(target, ".wal")))
  con <- dbConnect(duckdb(), target, array = "matrix")
  dbWriteTable(con, "nodes", nodes, overwrite = TRUE)
  dbWriteTable(con, "edges", edges, overwrite = TRUE)
  dbExecute(con, "CREATE INDEX idx_edges_source ON edges(source)")
  dbExecute(con, "CREATE INDEX idx_edges_target ON edges(target)")
  dbExecute(con, "CREATE INDEX idx_nodes_kind ON nodes(kind)")
  dbDisconnect(con, shutdown = TRUE)
  unlink(paste0(target, ".wal"))
  if (!file.rename(target, path))
    stop("graph built at ", target, " but could not replace ", path, call. = FALSE)

  if (csv) {
    write.csv(nodes, file.path(dirname(cfg$catalog), "policy_nodes.csv"), row.names = FALSE)
    write.csv(edges, file.path(dirname(cfg$catalog), "policy_edges.csv"), row.names = FALSE)
  }

  message("nodes: ", nrow(nodes), " (",
          paste(sprintf("%s %d", names(table(nodes$kind)), table(nodes$kind)),
                collapse = ", "), ")")
  message("edges: ", nrow(edges), " (",
          paste(sprintf("%s %d", names(table(edges$relationship_type)),
                        table(edges$relationship_type)), collapse = ", "), ")")
  message("wrote ", path)
  invisible(list(nodes = nodes, edges = edges))
}
