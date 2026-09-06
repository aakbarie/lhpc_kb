# Tier 1 fetcher: Readily public policy repositories.
#
# Generalized from policy_search/R/fetch.R — the only per-plan variation is
# the org slug, verified 2026-09-06 when CHPIV's payload came back
# schema-identical to the Alliance's:
#
#   GET {api}/org/{org}/documents        -> catalog with metadata
#   GET {api}/policy/{public_link_id}    -> that policy's PDF bytes

READILY_API <- Sys.getenv("KB_READILY_API",
                          "https://api.readily.co/api/v1/policy-public")

#' One plan's Readily repository as unified catalog rows. Policies with no
#' public PDF (Word-only, attachments only) are dropped here rather than
#' failing later at download time.
readily_catalog <- function(plan) {
  url <- sprintf("%s/org/%s/documents", READILY_API, plan$readily$org)
  message("[", plan$plan_id, "] fetching Readily repository: ", url)
  body <- kb_request(url, kb_ua(plan)) |> req_perform() |> resp_body_json()
  message(sprintf("  %s: %d policies, %d documents",
                  body$organization_name, body$policy_count, body$total_document_count))

  lob <- paste(plan$lines_of_business %||% "medi-cal", collapse = ",")
  rows <- lapply(body$policies, function(p) {
    pdfs <- p$pdf_documents
    if (length(pdfs) == 0) return(NULL)
    doc <- pdfs[[1]]
    catalog_row(
      plan_id          = plan$plan_id,
      line_of_business = lob,
      doc_id           = doc$public_link_id,
      policy_number    = p$policy_number,
      title            = p$policy_title,
      department       = p$lead_department %||% "Unspecified",
      status           = p$policy_status %||% "Unknown",
      date_published   = substr(p$date_published %||% "", 1, 10),
      due_date         = substr(p$due_date %||% "", 1, 10),
      doc_kind         = "policy",
      source_url       = sprintf("%s/policy/%s", READILY_API, doc$public_link_id),
      filename         = paste0(safe_name(paste(p$policy_number, p$policy_title)), ".pdf")
    )
  })
  dedupe_filenames(do.call(rbind, Filter(Negate(is.null), rows)))
}

readily_fetch <- function(plan, limit = NULL, refresh = FALSE) {
  cat <- readily_catalog(plan)
  if (!is.null(limit)) cat <- head(cat, limit)
  fetch_documents(plan, cat, refresh)
}
