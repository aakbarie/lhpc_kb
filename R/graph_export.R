# Export a compact view of the graph for visualisation.
#
# Not the whole graph: 2,793 nodes force-laid-out is a hairball that says
# nothing. What is worth drawing is the part that carries information —
# the supersession lineages (which are temporal, so they can be positioned
# by year rather than by a physics simulation) and the strongest cross-plan
# links (which are the argument for an LHPC-wide corpus in the first place).
#
#   Rscript -e 'source("R/graph_export.R"); export_graph_json()'

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(jsonlite)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "config.R", "graph.R"))
    source(file.path(d, "R", f))
})

#' APL "26-014" -> 2026, "98-009" -> 1998. DHCS has used two-digit years since
#' 1998, so the century pivot is unambiguous for the life of this corpus.
apl_year <- function(key) {
  yy <- as.integer(substr(key, 1, 2))
  ifelse(yy >= 90, 1900L + yy, 2000L + yy)
}

export_graph_json <- function(cfg = kb_config(),
                              out = file.path(dirname(cfg$catalog), "graph_view.json"),
                              max_pairs = 60) {
  con <- dbConnect(duckdb(), graph_path(cfg), read_only = TRUE, array = "matrix")
  on.exit(dbDisconnect(con, shutdown = TRUE))

  counts_nodes <- dbGetQuery(con, "SELECT kind, count(*) AS n FROM nodes GROUP BY 1 ORDER BY 2 DESC")
  counts_edges <- dbGetQuery(con, "SELECT relationship_type AS type, count(*) AS n FROM edges GROUP BY 1 ORDER BY 2 DESC")

  per_plan <- dbGetQuery(con, "
    SELECT p.plan_id, p.label AS plan, p.sublabel AS domain,
           count(d.id) FILTER (WHERE d.kind = 'document') AS documents
    FROM nodes p LEFT JOIN nodes d ON d.plan_id = p.plan_id AND d.kind = 'document'
    WHERE p.kind = 'plan' GROUP BY 1,2,3 ORDER BY documents DESC")

  sup <- dbGetQuery(con, "
    SELECT replace(e.source,'apl:','') AS newer, replace(e.target,'apl:','') AS older
    FROM edges e WHERE e.relationship_type = 'supersedes'")
  apl_lab <- dbGetQuery(con, "
    SELECT replace(id,'apl:','') AS apl, label FROM nodes WHERE kind = 'apl'")

  # Resolve edges into maximal chains: start from every APL that nothing
  # supersedes (a chain head) and walk down. A letter can supersede several,
  # so this is a DAG, not a list — walking the longest branch keeps each
  # chain readable while `sup` retains the full structure for the tooltip.
  heads <- setdiff(sup$newer, sup$older)
  walk <- function(node, seen = character()) {
    if (node %in% seen) return(node)
    nxt <- sup$older[sup$newer == node]
    if (!length(nxt)) return(node)
    best <- NULL
    for (n in nxt) {
      cand <- walk(n, c(seen, node))
      if (is.null(best) || length(cand) > length(best)) best <- cand
    }
    c(node, best)
  }
  chains <- lapply(heads, function(h) walk(h))
  chains <- chains[lengths(chains) > 1]
  chains <- chains[order(-lengths(chains))]

  chain_json <- lapply(chains, function(ch) {
    list(members = lapply(ch, function(k) list(
      apl = k, year = apl_year(k),
      label = (function(l) if (length(l) && !is.na(l)) l else paste("APL", k))(
        apl_lab$label[match(k, apl_lab$apl)]))))
  })

  pairs <- dbGetQuery(con, sprintf("
    SELECT a.plan_id AS plan_a, substr(a.label,1,72) AS doc_a,
           b.plan_id AS plan_b, substr(b.label,1,72) AS doc_b,
           e.weight AS shared
    FROM edges e JOIN nodes a ON a.id=e.source JOIN nodes b ON b.id=e.target
    WHERE e.relationship_type='similar_topic'
    ORDER BY e.weight DESC, doc_a LIMIT %d", max_pairs))

  implements <- dbGetQuery(con, "
    SELECT replace(e.target,'apl:','') AS apl,
           substr(t.label,1,72) AS subject,
           count(DISTINCT n.plan_id) AS plans, count(*) AS documents
    FROM edges e JOIN nodes n ON n.id=e.source JOIN nodes t ON t.id=e.target
    WHERE e.relationship_type='implements'
    GROUP BY 1,2 ORDER BY plans DESC, documents DESC LIMIT 25")

  catalog <- read_catalog()
  payload <- list(
    generated   = format(Sys.Date()),
    totals      = list(documents = nrow(catalog),
                       plans = sum(per_plan$documents > 0),
                       nodes = sum(counts_nodes$n), edges = sum(counts_edges$n)),
    node_counts = counts_nodes, edge_counts = counts_edges,
    plans       = per_plan, chains = chain_json,
    cross_plan  = pairs, implements = implements)

  write_json(payload, out, auto_unbox = TRUE, digits = NA)
  message("wrote ", out, " (", length(chain_json), " chains, ",
          nrow(pairs), " cross-plan pairs)")
  invisible(payload)
}
