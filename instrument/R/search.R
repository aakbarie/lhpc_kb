# Policy search over state guidance.
#
# SCOPE IS A PRODUCT DECISION, not a technical one. This page ships with the
# instrument, so it may only contain material the instrument's buyer does not
# own: DHCS All Plan Letters are public state guidance and travel with the
# product; a plan's own policy library is tenant data and does not. The filter
# is therefore hard-coded to plan_id = 'dhcs_apl' rather than configurable —
# a deployment that could widen it by setting a flag is a deployment that will.
#
# Within that, the corpus is narrowed to the letters this instrument is about.
# 133 APLs mention a grievance or an appeal somewhere; most are incidental
# (a Whole Child Model letter noting that appeal rights exist). Scope is by
# DENSITY instead — the share of a letter that is about grievances — which
# promotes a short letter wholly on the subject over a long one that mentions
# it in passing, and yields 37.
#
# Ranking is BM25, computed over the scoped subset rather than through
# DuckDB's FTS index. The index is present and the function works, but
# match_bm25() is evaluated per row across all 106,417 chunks BEFORE any
# filter can apply — a scoped query still pays for the whole store, and the
# first attempt here did not return inside five minutes. policy_search hit
# the same wall from the other side and disabled its BM25 lane outright.
# The scope is ~800 chunks, so scoring them directly is both faster and
# under our control: proper IDF, term frequency saturation and length
# normalisation, on a set small enough that it costs milliseconds.

suppressPackageStartupMessages({ library(DBI); library(duckdb) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  inst <- file.path(d, "instrument"); if (!dir.exists(file.path(inst, "R"))) inst <- d
  for (f in c("fetch_common.R", "config.R")) source(file.path(d, "R", f))
  for (f in c("passport.R", "policies.R")) source(file.path(inst, "R", f))
})

SEARCH_SOURCE <- "dhcs_apl"          # state guidance only — see header
GRIEVANCE_RE  <- "grievance|appeal"
MIN_DENSITY   <- 0.15
MIN_CHUNKS    <- 3

#' The letters in scope, with how much of each is about grievances and
#' whether anything has superseded it.
grievance_scope <- function(cfg = kb_config()) {
  if (!file.exists(cfg$store_path)) return(NULL)
  con <- tryCatch(dbConnect(duckdb(), cfg$store_path, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  out <- tryCatch(dbGetQuery(con, sprintf("
    WITH t AS (SELECT policy_number, any_value(title) AS title,
                      any_value(filename) AS filename, count(*) AS total
               FROM chunks WHERE plan_id='%s' GROUP BY 1),
         g AS (SELECT policy_number, count(*) AS hits FROM chunks
               WHERE plan_id='%s' AND regexp_matches(lower(text),'%s') GROUP BY 1)
    SELECT t.policy_number, t.title, t.filename, g.hits, t.total,
           g.hits::DOUBLE / t.total AS density
    FROM t JOIN g ON g.policy_number = t.policy_number
    WHERE g.hits >= %d AND g.hits::DOUBLE / t.total >= %f
    ORDER BY density DESC", SEARCH_SOURCE, SEARCH_SOURCE, GRIEVANCE_RE,
    MIN_CHUNKS, MIN_DENSITY)), error = function(e) NULL)
  if (is.null(out) || !nrow(out)) return(NULL)

  out$year <- 1900L + as.integer(substr(sub("^APL ", "", out$policy_number), 1, 2))
  out$year[out$year < 1990] <- out$year[out$year < 1990] + 100L
  out$status <- ifelse(out$policy_number %in% superseded_apls(cfg),
                       "Superseded", "Current")
  out
}

#' APLs something else has replaced, from the supersession graph.
superseded_apls <- function(cfg = kb_config()) {
  g <- graph_db(cfg)
  if (!file.exists(g)) return(character())
  con <- tryCatch(dbConnect(duckdb(), g, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(character())
  on.exit(dbDisconnect(con, shutdown = TRUE))
  r <- tryCatch(dbGetQuery(con,
    "SELECT DISTINCT replace(target,'apl:','') AS a FROM edges
      WHERE relationship_type='supersedes'"), error = function(e) NULL)
  if (is.null(r) || !nrow(r)) return(character())
  paste("APL", r$a)
}

#' Okapi BM25 over the scoped chunks.
#'
#' k1 and b are the standard defaults. Length normalisation matters here
#' because a table of timeframes and a page of preamble differ by an order of
#' magnitude in length, and without it the preamble wins on raw counts.
BM25_K1 <- 1.2
BM25_B  <- 0.75

apl_search <- function(query, scope = grievance_scope(), apls = NULL,
                       status = NULL, years = NULL, k = 25, cfg = kb_config()) {
  q <- trimws(query %||% "")
  if (!nzchar(q) || is.null(scope) || !nrow(scope)) return(NULL)
  con <- tryCatch(dbConnect(duckdb(), cfg$store_path, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  keep <- scope
  if (!is.null(status) && length(status)) keep <- keep[keep$status %in% status, ]
  if (!is.null(years)  && length(years))  keep <- keep[keep$year %in% years, ]
  if (!is.null(apls)   && length(apls))   keep <- keep[keep$policy_number %in% apls, ]
  if (!nrow(keep)) return(NULL)
  inlist <- paste(sprintf("'%s'", gsub("'", "''", keep$policy_number)), collapse = ",")

  docs <- tryCatch(dbGetQuery(con, sprintf(
    "SELECT policy_number, filename, page, text FROM chunks
      WHERE plan_id = '%s' AND policy_number IN (%s)", SEARCH_SOURCE, inlist)),
    error = function(e) NULL)
  if (is.null(docs) || !nrow(docs)) return(NULL)

  terms <- unique(unlist(strsplit(tolower(gsub("[^a-z0-9 ]+", " ", tolower(q))), " +")))
  terms <- terms[nchar(terms) > 2]
  if (!length(terms)) return(NULL)
  # Prefix matching for the same reason as the assistant: the requirement
  # sits in a table saying "Acknowledgment", and "acknowledge" is not a
  # substring of it.
  stems <- substr(terms, 1, 7)

  body <- tolower(docs$text)
  len  <- nchar(body); avg <- mean(len)
  N <- length(body)
  score <- numeric(N)
  for (st in unique(stems)) {
    tf <- vapply(gregexpr(st, body, fixed = TRUE),
                 function(m) if (m[1] == -1) 0 else length(m), 0)
    df <- sum(tf > 0)
    if (df == 0) next
    idf <- log(1 + (N - df + 0.5) / (df + 0.5))
    score <- score + idf * (tf * (BM25_K1 + 1)) /
             (tf + BM25_K1 * (1 - BM25_B + BM25_B * len / avg))
  }
  ord <- order(score, decreasing = TRUE)
  ord <- ord[score[ord] > 0]
  if (!length(ord)) return(NULL)
  res <- docs[head(ord, k), , drop = FALSE]
  res$score <- score[head(ord, k)]

  m <- match(res$policy_number, scope$policy_number)
  res$title  <- scope$title[m]
  res$status <- scope$status[m]
  res$year   <- scope$year[m]
  res$snippet <- vapply(res$text, snippet_for, "", query = q, USE.NAMES = FALSE)
  res$text <- NULL
  res
}

#' A window of text around the first query term, with the terms marked.
#'
#' Returns HTML, so the caller must render it as such — and everything that
#' reaches here is corpus text plus the user's own query, both escaped before
#' the marks go in.
snippet_for <- function(text, query, width = 320) {
  terms <- unique(unlist(strsplit(tolower(gsub("[^a-z0-9 ]+", " ", tolower(query))), " +")))
  terms <- terms[nchar(terms) > 2]
  flat <- gsub("[[:space:]]+", " ", text)
  at <- NA_integer_
  for (t in terms) {
    p <- regexpr(t, tolower(flat), fixed = TRUE)
    if (p > 0) { at <- p; break }
  }
  start <- if (is.na(at)) 1L else max(1L, at - 90L)
  frag <- substr(flat, start, start + width)
  frag <- htmltools::htmlEscape(frag)
  for (t in terms) {
    # Escape with perl = TRUE. R's default TRE engine rejects this character
    # class outright ("Invalid contents of {}"), which is the same trap the
    # robots matcher hit — worth escaping the escaper carefully rather than
    # discovering it again.
    esc <- gsub("([][{}().^$+*?|\\\\])", "\\\\\\1", t, perl = TRUE)
    frag <- gsub(sprintf("(%s)", esc), "<mark>\\1</mark>", frag,
                 perl = TRUE, ignore.case = TRUE)
  }
  paste0(if (start > 1) "…", frag, "…")
}

#' Counts for the facet rail, computed over the unfiltered result set so a
#' facet always shows what selecting it would yield.
search_facets <- function(query, scope = grievance_scope(), cfg = kb_config()) {
  hits <- apl_search(query, scope, k = 500, cfg = cfg)
  if (is.null(hits)) return(NULL)
  list(
    status = sort(table(hits$status), decreasing = TRUE),
    year   = sort(table(hits$year), decreasing = TRUE),
    apl    = sort(table(hits$policy_number), decreasing = TRUE),
    total  = nrow(hits))
}
