# Shared fetch machinery: paths, throttled requests, the unified catalog
# schema, and the guarded PDF download. Every network call in every fetcher
# goes through kb_request() so the rate limit and retry policy cannot drift
# apart between tiers.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Project root: the directory holding plans/registry.yml, found by walking
#' up from the working directory so scripts run from root, R/, or elsewhere.
kb_root <- function() {
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  if (!file.exists(file.path(d, "plans", "registry.yml")))
    stop("cannot find plans/registry.yml above ", getwd())
  d
}

kb_paths <- function() {
  root <- Sys.getenv("LHPC_DATA_DIR", file.path(kb_root(), "data"))
  list(pdf = file.path(root, "pdf"), catalog = file.path(root, "catalog.json"))
}

UA_DEFAULT <- "PolicyKB/1.0 (internal policy knowledge base; respectful crawl, 1 req/sec)"
# Some plans' CMSes 403 non-browser UAs while serving the same documents to
# browsers (survey note 1). Used only where the registry says user_agent: browser.
UA_BROWSER <- paste0("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
                     "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0 Safari/537.36")

kb_ua <- function(plan) {
  if (identical(plan$user_agent, "browser")) UA_BROWSER else UA_DEFAULT
}

kb_request <- function(url, ua = UA_DEFAULT, delay = 1) {
  # IEHP's DAM (and some sitemaps) publish URLs with literal spaces; a raw
  # space is never legal in a URL, so this is always safe to normalize.
  url <- gsub(" ", "%20", url, fixed = TRUE)
  req <- request(url) |>
    req_user_agent(ua) |>
    req_timeout(120) |>
    req_throttle(capacity = 1, fill_time_s = delay) |>
    req_retry(max_tries = 3)
  # Some servers (IEHP) emit HTTP/2 header frames libcurl rejects outright;
  # a plan sets `http1: true` in the registry and fetch_all() flips this
  # option for the duration of that plan's run. 2 = CURL_HTTP_VERSION_1_1.
  if (isTRUE(getOption("kb.http1"))) req <- req_options(req, http_version = 2)
  req
}

#' Filesystem-safe name that still reads like the document it came from.
safe_name <- function(x, max_len = 90) {
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  substr(x, 1, max_len)
}

# The unified catalog: policy_search's ten columns plus the two that make one
# store serve 18 corpora. Facets and citations key on plan_id; retrieval is
# unchanged.
CATALOG_COLS <- c("plan_id", "line_of_business",
                  "doc_id", "policy_number", "title", "department", "status",
                  "date_published", "due_date", "doc_kind", "source_url", "filename")

catalog_row <- function(...) {
  vals <- list(...)
  unknown <- setdiff(names(vals), CATALOG_COLS)
  if (length(unknown)) stop("not catalog columns: ", paste(unknown, collapse = ", "))
  row <- setNames(as.list(rep("", length(CATALOG_COLS))), CATALOG_COLS)
  row[names(vals)] <- lapply(vals, function(v) trimws(as.character(v %||% "")))
  as.data.frame(row, stringsAsFactors = FALSE)
}

empty_catalog <- function() {
  as.data.frame(setNames(rep(list(character()), length(CATALOG_COLS)), CATALOG_COLS))
}

#' The unified catalog as a data frame; an empty frame with the right columns
#' when absent, so callers can always rely on the shape. Lives here rather
#' than in fetch.R because ingest reads it too.
read_catalog <- function() {
  path <- kb_paths()$catalog
  if (!file.exists(path)) return(empty_catalog())
  got <- fromJSON(path)
  for (col in setdiff(CATALOG_COLS, names(got))) got[[col]] <- ""
  got[, CATALOG_COLS]
}

#' Download one PDF with the magic-byte guard. An HTML error page saved with a
#' .pdf name is invisible until ingest reports a document with no text, so it
#' is caught here instead. Idempotent unless `refresh`.
download_pdf <- function(url, dest, ua = UA_DEFAULT, delay = 1, refresh = FALSE,
                         cookie = "") {
  if (file.exists(dest) && !refresh && file.size(dest) > 0) return(TRUE)
  tryCatch({
    req <- kb_request(url, ua, delay)
    if (nzchar(cookie)) req <- req_headers(req, Cookie = cookie)
    bytes <- req |> req_perform() |> resp_body_raw()
    if (length(bytes) < 5 || !identical(rawToChar(bytes[1:4]), "%PDF")) {
      warning("not a PDF: ", url, call. = FALSE)
      return(FALSE)
    }
    writeBin(bytes, dest)
    TRUE
  }, error = function(e) {
    warning("failed ", basename(dest), ": ", conditionMessage(e), call. = FALSE)
    FALSE
  })
}

#' Two documents can slugify to the same filename; the doc id usually
#' disambiguates, but site doc_ids share a host prefix, so a counter backstop
#' guarantees uniqueness either way.
dedupe_filenames <- function(cat) {
  dup <- duplicated(cat$filename)
  cat$filename[dup] <- paste0(sub("\\.pdf$", "", cat$filename[dup]), "_",
                              substr(gsub("[^A-Za-z0-9]", "", cat$doc_id[dup]), 1, 6), ".pdf")
  if (anyDuplicated(cat$filename))
    cat$filename <- paste0(make.unique(sub("\\.pdf$", "", cat$filename), sep = "_"), ".pdf")
  cat
}
