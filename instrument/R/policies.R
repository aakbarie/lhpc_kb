# Applicable policies: the plan's own implementation of a state requirement.
#
# A gate cites an All Plan Letter, but an operator does not work from the APL —
# they work from their plan's policy implementing it, and an auditor asks to
# see that policy, at the page. The `implements` edges built in R/graph.R
# already carry the mapping (2,525 of them, mined from document text), so this
# is a lookup rather than new inference.
#
# PDFs are served read-only from the corpus. The page number comes from the
# chunk's page column, which is the same anchor the citation uses — so
# "APL 21-011 page 12" and the link that opens page 12 cannot disagree.

suppressPackageStartupMessages({ library(DBI); library(duckdb) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  inst <- file.path(d, "instrument"); if (!dir.exists(file.path(inst, "R"))) inst <- d
  for (f in c("fetch_common.R", "config.R")) source(file.path(d, "R", f))
  for (f in c("passport.R")) source(file.path(inst, "R", f))
})

graph_db <- function(cfg = kb_config())
  file.path(dirname(cfg$catalog), "lhpc_graph.duckdb")

#' Plan policies that implement a given authority.
#'
#' `plans` filters to one plan's own library; left NULL it shows the whole
#' membership, which is what makes "how do the others handle this" answerable
#' from inside a case.
applicable_policies <- function(authority, plans = NULL, limit = 12,
                                cfg = kb_config()) {
  g <- graph_db(cfg)
  if (!file.exists(g) || !nzchar(authority %||% "")) return(NULL)
  key <- paste0("apl:", gsub("[^0-9-]", "", authority))

  con <- tryCatch(dbConnect(duckdb(), g, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  out <- tryCatch(dbGetQuery(con, "
      SELECT n.plan_id, n.label, n.sublabel AS filename, e.weight AS mentions
      FROM edges e JOIN nodes n ON n.id = e.source
      WHERE e.relationship_type = 'implements' AND e.target = ?
        AND n.kind = 'document'
      ORDER BY e.weight DESC", params = list(key)), error = function(e) NULL)
  if (is.null(out) || !nrow(out)) return(NULL)
  if (!is.null(plans)) out <- out[out$plan_id %in% plans, , drop = FALSE]
  head(out, limit)
}

#' Where a term appears inside one document — the page a link should open at.
policy_page_for <- function(plan_id, filename, terms, cfg = kb_config()) {
  if (!file.exists(cfg$store_path)) return(NA_integer_)
  con <- tryCatch(dbConnect(duckdb(), cfg$store_path, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NA_integer_)
  on.exit(dbDisconnect(con, shutdown = TRUE))
  rx <- paste0("(?i)(", paste(substr(terms, 1, 7), collapse = "|"), ")")
  r <- tryCatch(dbGetQuery(con, "
      SELECT page FROM chunks
      WHERE plan_id = ? AND filename = ? AND regexp_matches(text, ?)
      ORDER BY page LIMIT 1",
      params = list(plan_id, filename, rx)), error = function(e) NULL)
  if (is.null(r) || !nrow(r)) NA_integer_ else as.integer(r$page[1])
}

#' Browser-openable URL for a corpus PDF, at a page.
#'
#' Relies on addResourcePath("corpus", data/pdf) in the app. The #page anchor
#' is honoured by every built-in PDF viewer, so this needs no pdf.js — and a
#' plain link degrades to opening page 1 rather than failing, if a viewer
#' ignores it.
pdf_url <- function(plan_id, filename, page = NA) {
  if (!nzchar(plan_id %||% "") || !nzchar(filename %||% "")) return(NULL)
  paste0("corpus/", utils::URLencode(plan_id), "/", utils::URLencode(filename),
         if (!is.na(page)) paste0("#page=", as.integer(page)) else "")
}

#' Does the file actually exist? A dead link in an audit tool is worse than a
#' missing one, because it looks like evidence until someone clicks it.
pdf_exists <- function(plan_id, filename, cfg = kb_config())
  file.exists(file.path(cfg$pdf_dir, plan_id, filename))
