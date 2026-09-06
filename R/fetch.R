# Orchestration: fetch any set of plans and maintain the unified catalog.
#
#   Rscript -e 'source("R/fetch.R"); fetch_all()'                      # all implemented
#   Rscript -e 'source("R/fetch.R"); fetch_all("chpiv", limit = 5)'    # smoke run
#   Rscript -e 'source("R/fetch.R"); fetch_all(c("partnership"))'
#
# The catalog is merge-by-plan: a run replaces the rows of the plans it
# fetched and leaves every other plan's rows alone, so plans can be fetched
# (and re-fetched on their own cadence) independently.

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "fetch_readily.R", "fetch_powerdms.R",
              "fetch_site.R", "fetch_manifest.R"))
    source(file.path(d, "R", f))
})

#' Download every document in `cat` into data/pdf/<plan_id>/, returning only
#' the rows whose PDF is actually on disk — so downstream ingest can trust
#' every row it reads.
fetch_documents <- function(plan, cat, refresh = FALSE, cookie = "") {
  dir <- file.path(kb_paths()$pdf, plan$plan_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  delay <- plan$crawl_delay %||% 1

  message("[", plan$plan_id, "] downloading ", nrow(cat), " documents into ", dir)
  keep <- logical(nrow(cat))
  for (i in seq_len(nrow(cat))) {
    keep[i] <- download_pdf(cat$source_url[i], file.path(dir, cat$filename[i]),
                            ua = kb_ua(plan), delay = delay, refresh = refresh,
                            cookie = cookie)
    if (i %% 25 == 0) message("  ", i, "/", nrow(cat))
  }
  cat[keep, , drop = FALSE]
}

write_catalog <- function(cat) {
  path <- kb_paths()$catalog
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(cat[order(cat$plan_id, cat$policy_number, cat$title), ],
             path, pretty = TRUE, auto_unbox = TRUE)
  message("wrote ", path, " (", nrow(cat), " documents, ",
          length(unique(cat$plan_id)), " plans)")
}

fetch_all <- function(plan_ids = NULL, limit = NULL, refresh = FALSE) {
  plans <- kb_fetchable(plan_ids)
  message("fetching ", length(plans), " plan(s): ",
          paste(names(plans), collapse = ", "))

  fetched <- lapply(plans, function(plan) {
    old <- options(kb.http1 = isTRUE(plan$http1)); on.exit(options(old))
    switch(plan$fetcher,
      readily  = readily_fetch(plan, limit, refresh),
      powerdms = powerdms_fetch(plan, limit, refresh),
      site     = site_fetch(plan, limit, refresh),
      manifest = manifest_fetch(plan, limit, refresh),
      stop("no fetcher: ", plan$fetcher)   # kb_fetchable should prevent this
    )
  })
  new_rows <- do.call(rbind, c(fetched, list(empty_catalog())))

  old <- read_catalog()
  merged <- rbind(old[!old$plan_id %in% names(plans), , drop = FALSE], new_rows)
  write_catalog(merged)
  invisible(merged)
}
