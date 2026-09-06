# Graph-augmented retrieval.
#
# Vector search finds passages that look like the question. On a corpus of 18
# plans writing about the same obligations, that is exactly the wrong
# similarity to trust on its own: the top-k is routinely eight passages from
# one plan, because that plan's phrasing happens to match, and the answer
# then reads as if it were the whole membership's position.
#
# The graph fixes a different problem than reranking does. Reranking reorders
# what vector search already found; graph expansion reaches documents vector
# search never surfaced — the other plans' version of the same rule, the
# state letter both are implementing, the letter that superseded it. Those
# are one or two hops away in the policy graph and arbitrarily far away in
# embedding space.
#
# This is where the small-world work pays off. Expansion is only useful with
# a small hop budget (2 hops; 3 already pulls in most of a cluster), so
# whether it reaches another plan at all is a property of the graph's
# topology, not of the query. See R/smallworld.R — before shortcuts, most
# document pairs cannot reach each other at any hop count.
#
# Usage:
#   Rscript -e 'source("R/retrieve_graph.R"); print(retrieve_graph("prior authorization timeframes"))'
#   Rscript -e 'source("R/retrieve_graph.R"); print(sw_retrieval_gain())'

suppressPackageStartupMessages({
  library(ragnar); library(igraph); library(DBI); library(duckdb)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "config.R", "graph.R", "smallworld.R"))
    source(file.path(d, "R", f))
})

#' Documents within `hops` of the seeds, along meaning-carrying edges only.
#'
#' published_by/owned_by are excluded for the same reason they are excluded
#' from the small-world measurement: they are hub stars, and one hop through
#' a plan node would "reach" every document that plan publishes. That is not
#' relatedness, it is a table scan wearing a graph's clothes.
graph_neighbors <- function(doc_ids, hops = 2, cfg = kb_config(),
                            types = SEMANTIC_EDGES, max_out = 40) {
  g <- tryCatch(sw_graph(cfg, types), error = function(e) NULL)
  if (is.null(g)) return(data.frame())
  seeds <- intersect(doc_ids, V(g)$name)
  if (!length(seeds)) return(data.frame())

  d <- distances(g, v = seeds)
  best <- apply(d, 2, min)
  hit <- names(best)[is.finite(best) & best > 0 & best <= hops]
  if (!length(hit)) return(data.frame())

  out <- data.frame(id = hit, hops = as.integer(best[hit]), stringsAsFactors = FALSE)
  out <- out[order(out$hops), , drop = FALSE]
  out <- head(out, max_out)
  out$plan_id <- sub("^doc:([^/]+)/.*$", "\\1", out$id)
  out$filename <- sub("^doc:[^/]+/", "", out$id)
  out[out$id != "" & startsWith(out$id, "doc:") | startsWith(out$id, "apl:"), , drop = FALSE]
}

#' Vector search over the chunk store, collapsed to one row per document.
vector_seeds <- function(query, top_k = 8, cfg = kb_config()) {
  if (!file.exists(cfg$store_path)) stop("no chunk store — run ingest_all() first")
  store <- ragnar_store_connect(cfg$store_path, read_only = TRUE)
  res <- ragnar_retrieve(store, query, top_k = top_k * 4, deoverlap = FALSE)
  if (!nrow(res)) return(data.frame())
  res$id <- paste0("doc:", file.path(res$plan_id, res$filename))
  keep <- !duplicated(res$id)
  head(res[keep, ], top_k)
}

#' Vector seeds, plus what the graph reaches from them.
#'
#' Returns both layers labelled, rather than one merged ranking: a passage
#' that was retrieved and a document that was reached are different kinds of
#' evidence, and collapsing them would let a graph neighbour be cited as if
#' the retriever had found it.
retrieve_graph <- function(query, top_k = 8, hops = 2, cfg = kb_config()) {
  seeds <- vector_seeds(query, top_k, cfg)
  if (!nrow(seeds)) return(list(seeds = seeds, reached = data.frame()))

  reached <- graph_neighbors(seeds$id, hops = hops, cfg = cfg)
  if (nrow(reached)) {
    reached <- reached[!reached$id %in% seeds$id, , drop = FALSE]
    cat <- read_catalog()
    key <- paste0("doc:", file.path(cat$plan_id, cat$filename))
    m <- match(reached$id, key)
    reached$title <- ifelse(is.na(m), reached$id, cat$title[m])
    reached$policy_number <- ifelse(is.na(m), "", cat$policy_number[m])
  }
  list(
    seeds = seeds[, intersect(c("plan_id", "policy_number", "title", "filename"),
                              names(seeds)), drop = FALSE],
    reached = reached)
}

#' How much cross-plan coverage does graph expansion actually buy?
#'
#' The number that justifies the whole apparatus. For each probe: how many
#' distinct plans does vector search alone surface, and how many after two
#' hops? If the graph is fragmented the second number barely moves, which is
#' precisely what the small-world augmentation is for.
SW_PROBES <- c(
  "prior authorization timeframes",
  "grievance and appeal resolution",
  "credentialing requirements for providers",
  "skilled nursing facility long term care",
  "non-emergency medical transportation",
  "initial health appointment for new members",
  "utilization management criteria disclosure",
  "behavioral health treatment for members under 21")

sw_retrieval_gain <- function(queries = SW_PROBES, top_k = 8, hops = 2,
                              cfg = kb_config()) {
  rows <- lapply(queries, function(q) {
    r <- tryCatch(retrieve_graph(q, top_k, hops, cfg), error = function(e) NULL)
    if (is.null(r) || !nrow(r$seeds)) return(NULL)
    vp <- length(unique(r$seeds$plan_id))
    gp <- length(unique(c(r$seeds$plan_id,
                          if (nrow(r$reached)) r$reached$plan_id else character())))
    data.frame(query = substr(q, 1, 42), vector_plans = vp, with_graph = gp,
               reached_docs = nrow(r$reached), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(out)) return(data.frame())
  attr(out, "mean_gain") <- mean(out$with_graph - out$vector_plans)
  out
}
