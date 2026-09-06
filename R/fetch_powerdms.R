# Tier 1 fetcher: PowerDMS public sites (Partnership HealthPlan).
#
#   GET https://public.powerdms.com/{site}/documents   Accept: application/json
#     -> { data: [ { id, name, publicUrl, breadcrumbs[0].trail[] } ] }
#   GET {publicUrl}                                    -> the PDF bytes
#
# Verified 2026-09-06: 454 documents; trail[1] is the department folder
# ("*Policy & Procedures" / <department> / <policy folder>); names lead with
# the policy code (CMP45, MPCP2007, LAU03 ...).
#
# NOTE: we fetch from public.powerdms.com only — partnershiphp.org's
# robots.txt disallows AI crawlers and is not touched (see registry notes).

POWERDMS_BASE <- "https://public.powerdms.com"

# "CMP45 Administrative..." / "MPCP2007  Complex Case Management" — a leading
# run of capitals then digits (optionally a trailing letter) is the code.
POWERDMS_CODE_RE <- "^([A-Z]{2,6}[0-9]{1,4}[A-Za-z]?)\\b\\s*"

powerdms_department <- function(doc) {
  trail <- tryCatch(doc$breadcrumbs[[1]]$trail, error = function(e) NULL)
  if (length(trail) >= 2) trail[[2]]$name %||% "Unspecified" else "Unspecified"
}

powerdms_catalog <- function(plan) {
  url <- sprintf("%s/%s/documents", POWERDMS_BASE, plan$powerdms$site)
  message("[", plan$plan_id, "] fetching PowerDMS manifest: ", url)
  body <- kb_request(url, kb_ua(plan)) |>
    req_headers(Accept = "application/json") |>
    req_perform() |> resp_body_json()

  docs <- body$data
  # Schema canary: an undocumented endpoint that changes shape should fail
  # loudly here, not produce 454 rows of blanks.
  if (length(docs) == 0 ||
      !all(c("id", "name", "publicUrl") %in% names(docs[[1]])))
    stop("PowerDMS manifest schema changed for site ", plan$powerdms$site)
  message("  ", length(docs), " documents")

  lob <- paste(plan$lines_of_business %||% "medi-cal", collapse = ",")
  rows <- lapply(docs, function(d) {
    name <- trimws(d$name %||% "")
    code <- if (grepl(POWERDMS_CODE_RE, name)) sub(paste0(POWERDMS_CODE_RE, ".*$"), "\\1", name) else ""
    catalog_row(
      plan_id          = plan$plan_id,
      line_of_business = lob,
      doc_id           = as.character(d$id),
      policy_number    = code,
      title            = sub(POWERDMS_CODE_RE, "", name),
      department       = powerdms_department(d),
      doc_kind         = "policy",
      source_url       = d$publicUrl,
      filename         = paste0(safe_name(name), ".pdf")
    )
  })
  dedupe_filenames(do.call(rbind, rows))
}

powerdms_fetch <- function(plan, limit = NULL, refresh = FALSE) {
  cat <- powerdms_catalog(plan)
  if (!is.null(limit)) cat <- head(cat, limit)
  fetch_documents(plan, cat, refresh)
}
