# Manifest fetcher: browser-required sources (DHCS behind Incapsula, CCHP
# behind Akamai) whose document lists were enumerated in a real browser
# session and committed as plans/manifests/<plan_id>.json.
#
# The manifest is the catalog; this fetcher only downloads. Sites that
# challenge plain HTTP clients accept requests carrying the browser session's
# cookie — set KB_COOKIE_<PLAN_ID> (uppercased) for the run, harvested from
# the same session that did the enumeration. Re-enumeration is a browser
# task; see each manifest's `method` field for the recorded workflow.

manifest_path <- function(plan)
  file.path(kb_root(), "plans", "manifests", paste0(plan$plan_id, ".json"))

manifest_cookie <- function(plan)
  Sys.getenv(paste0("KB_COOKIE_", toupper(plan$plan_id)), unset = "")

manifest_catalog <- function(plan) {
  path <- manifest_path(plan)
  if (!file.exists(path))
    stop("[", plan$plan_id, "] no manifest at ", path,
         " — enumerate in a browser first (see registry notes)")
  m <- fromJSON(path, simplifyDataFrame = FALSE)
  message("[", plan$plan_id, "] manifest of ", length(m$documents),
          " documents (harvested ", m$harvested %||% "?", ")")

  lob_default <- paste(plan$lines_of_business %||% "medi-cal", collapse = ",")
  rows <- lapply(m$documents, function(d) {
    catalog_row(
      plan_id          = plan$plan_id,
      line_of_business = d$line_of_business %||% lob_default,
      doc_id           = sub("^https?://", "", d$source_url),
      policy_number    = d$policy_number %||% "",
      title            = d$title %||% "",
      department       = d$department %||% "Unspecified",
      date_published   = d$date_published %||% "",
      doc_kind         = d$doc_kind %||% "policy",
      source_url       = d$source_url,
      filename         = paste0(safe_name(paste(d$policy_number %||% "", d$title %||% "")), ".pdf")
    )
  })
  dedupe_filenames(do.call(rbind, rows))
}

manifest_fetch <- function(plan, limit = NULL, refresh = FALSE) {
  cat <- manifest_catalog(plan)
  if (!is.null(limit)) cat <- head(cat, limit)
  cookie <- manifest_cookie(plan)
  if (!nzchar(cookie))
    message("  no KB_COOKIE_", toupper(plan$plan_id),
            " set — proceeding without; challenged sites will fail the %PDF guard")
  fetch_documents(plan, cat, refresh, cookie = cookie)
}
