# Questions you can only ask a graph.
#
# Each of these is a traversal that hybrid search cannot do, because search
# ranks passages and these need paths: which plans adopted a state
# requirement, which policies cite a letter that has since been replaced,
# what every plan's version of one requirement looks like side by side.
#
# Read-only throughout — many callers can hold the graph at once.
#
# Usage:
#   Rscript -e 'source("R/graph_query.R"); print(apl_adoption("26-014"))'
#   Rscript -e 'source("R/graph_query.R"); print(stale_citations(), n = 20)'

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "config.R", "graph.R"))
    source(file.path(d, "R", f))
})

graph_con <- function(cfg = kb_config()) {
  p <- graph_path(cfg)
  if (!file.exists(p)) stop("No graph at ", p, " — run build_graph() first.")
  dbConnect(duckdb(), p, read_only = TRUE, array = "matrix")
}

#' Normalise "APL 26-014" / "26-014" / "apl26-014" to the node key "26-014".
as_apl_key <- function(x) {
  k <- apl_ids(x)[[1]]
  if (!length(k)) stop("not an APL identifier: ", x)
  k[1]
}

#' Which plans have a document citing this APL — and, as importantly, which
#' have not. The absent rows are the finding: a state requirement with no
#' implementing policy at a plan is either a gap or a corpus gap, and both are
#' worth knowing. `documents` is 0 for plans that never mention it.
apl_adoption <- function(apl, cfg = kb_config()) {
  con <- graph_con(cfg); on.exit(dbDisconnect(con, shutdown = TRUE))
  key <- as_apl_key(apl)
  dbGetQuery(con, "
    WITH citing AS (
      SELECT n.plan_id, count(*) AS documents
      FROM edges e JOIN nodes n ON n.id = e.source
      WHERE e.relationship_type = 'implements' AND e.target = ?
      GROUP BY 1)
    SELECT p.label AS plan, coalesce(c.documents, 0) AS documents
    FROM nodes p LEFT JOIN citing c ON c.plan_id = p.plan_id
    WHERE p.kind = 'plan' AND p.plan_id <> 'dhcs_apl'
    ORDER BY documents DESC, plan", params = list(paste0("apl:", key)))
}

#' The full supersession lineage of an APL: what it replaced, transitively,
#' and what has replaced it. A plan policy citing anything below the head of
#' this chain is citing stale guidance.
apl_lineage <- function(apl, cfg = kb_config()) {
  con <- graph_con(cfg); on.exit(dbDisconnect(con, shutdown = TRUE))
  key <- paste0("apl:", as_apl_key(apl))
  older <- dbGetQuery(con, "
    WITH RECURSIVE d(node, depth) AS (
      SELECT ?, 0
      UNION ALL
      SELECT e.target, d.depth + 1 FROM d JOIN edges e ON e.source = d.node
      WHERE e.relationship_type = 'supersedes' AND d.depth < 12)
    SELECT DISTINCT replace(node,'apl:','') AS apl, depth FROM d WHERE depth > 0
    ORDER BY depth", params = list(key))
  newer <- dbGetQuery(con, "
    WITH RECURSIVE u(node, depth) AS (
      SELECT ?, 0
      UNION ALL
      SELECT e.source, u.depth + 1 FROM u JOIN edges e ON e.target = u.node
      WHERE e.relationship_type = 'supersedes' AND u.depth < 12)
    SELECT DISTINCT replace(node,'apl:','') AS apl, depth FROM u WHERE depth > 0
    ORDER BY depth", params = list(key))
  list(apl = sub("^apl:", "", key), supersedes = older, superseded_by = newer)
}

#' Plan policies citing an APL that has since been superseded.
#'
#' The compliance question this exists to answer: a policy quoting a letter
#' DHCS has replaced may be describing a rule that no longer applies. It is a
#' flag, not a verdict — a policy can legitimately cite an old letter as
#' history — so the replacement is reported alongside, for a human to judge.
stale_citations <- function(cfg = kb_config(), plan_ids = NULL) {
  con <- graph_con(cfg); on.exit(dbDisconnect(con, shutdown = TRUE))
  out <- dbGetQuery(con, "
    SELECT d.plan_id,
           substr(d.label, 1, 70) AS document,
           replace(e.target,'apl:','')  AS cites_apl,
           replace(s.source,'apl:','')  AS superseded_by
    FROM edges e
    JOIN nodes d ON d.id = e.source
    JOIN edges s ON s.target = e.target AND s.relationship_type = 'supersedes'
    WHERE e.relationship_type = 'implements'
    ORDER BY d.plan_id, cites_apl")
  if (!is.null(plan_ids)) out <- out[out$plan_id %in% plan_ids, , drop = FALSE]
  out
}

#' Every other plan's document on the same subject as this one.
#' The side-by-side view: same requirement, different plans.
cross_plan <- function(doc_id_substring, cfg = kb_config()) {
  con <- graph_con(cfg); on.exit(dbDisconnect(con, shutdown = TRUE))
  dbGetQuery(con, "
    SELECT a.plan_id AS plan, substr(a.label,1,60) AS document,
           b.plan_id AS other_plan, substr(b.label,1,60) AS other_document,
           e.weight AS shared_terms
    FROM edges e
    JOIN nodes a ON a.id = e.source
    JOIN nodes b ON b.id = e.target
    WHERE e.relationship_type = 'similar_topic'
      AND (lower(a.label) LIKE lower(?) OR lower(b.label) LIKE lower(?))
    ORDER BY e.weight DESC LIMIT 25",
    params = list(paste0("%", doc_id_substring, "%"),
                  paste0("%", doc_id_substring, "%")))
}

#' Which APLs are most widely implemented across the membership — the state
#' requirements the most plans have written policy against.
apl_reach <- function(cfg = kb_config(), limit = 20) {
  con <- graph_con(cfg); on.exit(dbDisconnect(con, shutdown = TRUE))
  dbGetQuery(con, "
    SELECT replace(e.target,'apl:','') AS apl,
           substr(t.label, 1, 60) AS subject,
           count(DISTINCT n.plan_id) AS plans,
           count(*) AS documents
    FROM edges e
    JOIN nodes n ON n.id = e.source
    JOIN nodes t ON t.id = e.target
    WHERE e.relationship_type = 'implements'
    GROUP BY 1, 2 ORDER BY plans DESC, documents DESC LIMIT ?",
    params = list(limit))
}

#' Corpus-level shape: node/edge counts, and igraph centrality over the
#' document-and-APL layer so the structurally central documents surface.
graph_summary <- function(cfg = kb_config(), top = 10) {
  con <- graph_con(cfg); on.exit(dbDisconnect(con, shutdown = TRUE))
  nodes <- dbGetQuery(con, "SELECT kind, count(*) AS n FROM nodes GROUP BY 1 ORDER BY 2 DESC")
  edges <- dbGetQuery(con, "SELECT relationship_type, count(*) AS n FROM edges GROUP BY 1 ORDER BY 2 DESC")
  central <- NULL
  if (requireNamespace("igraph", quietly = TRUE)) {
    el <- dbGetQuery(con, "
      SELECT source, target FROM edges
      WHERE relationship_type IN ('implements','supersedes','similar_topic')")
    if (nrow(el)) {
      g <- igraph::graph_from_data_frame(el, directed = FALSE)
      deg <- sort(igraph::degree(g), decreasing = TRUE)[seq_len(min(top, igraph::vcount(g)))]
      lab <- dbGetQuery(con, "SELECT id, plan_id, substr(label,1,60) AS label FROM nodes")
      central <- data.frame(id = names(deg), degree = as.integer(deg),
                            row.names = NULL)
      central$plan  <- lab$plan_id[match(central$id, lab$id)]
      central$label <- lab$label[match(central$id, lab$id)]
    }
  }
  list(nodes = nodes, edges = edges, most_connected = central)
}
