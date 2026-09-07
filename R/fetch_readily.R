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

#' Candidate org slugs for a plan.
#'
#' All three live orgs follow one rule — the organisation's full name,
#' lowercased and hyphenated ("Community Health Plan of Imperial Valley" ->
#' community-health-plan-of-imperial-valley) — so the slug is derivable
#' rather than something to look up. Variants are tried after it because a
#' plan may register under a shorter trading name.
readily_slugs <- function(plan) {
  if (!is.null(plan$readily$org)) return(plan$readily$org)
  base <- tolower(iconv(plan$name, to = "ASCII//TRANSLIT", sub = ""))
  base <- gsub("[^a-z0-9]+", "-", base)
  base <- gsub("^-+|-+$", "", base)
  unique(c(base,
           sub("^the-", "", base),
           gsub("-of-", "-", base),
           tolower(plan$plan_id)))
}

#' Which member plans are live on Readily, and which are only provisioned.
#'
#' The Readily API exposes no org directory (every listing endpoint 404s), so
#' this probes derived slugs. A "provisioned" result — an org that resolves
#' with zero documents — is the signature of a migration in progress, and is
#' the reason Community Health Group has no policy library anywhere else.
#'
#' Absence here means "not found under these slugs", never "not on Readily":
#' a plan may have registered a name we cannot guess, or not enabled the
#' public repository. Treat it as a prompt to ask the plan, not a finding.
readily_discover <- function(reg = kb_registry(), delay = 0.5) {
  rows <- lapply(reg, function(plan) {
    if (identical(plan$plan_id, "dhcs_apl")) return(NULL)
    for (slug in readily_slugs(plan)) {
      body <- tryCatch(
        kb_request(sprintf("%s/org/%s/documents", READILY_API, slug), delay = delay) |>
          req_perform() |> resp_body_json(),
        error = function(e) NULL)
      if (!is.null(body) && !is.null(body$organization_name))
        return(data.frame(
          plan_id = plan$plan_id, plan = plan$name, slug = slug,
          policies = body$policy_count %||% 0L,
          documents = body$total_document_count %||% 0L,
          state = if ((body$policy_count %||% 0L) > 0) "live" else "provisioned",
          stringsAsFactors = FALSE))
    }
    data.frame(plan_id = plan$plan_id, plan = plan$name, slug = NA_character_,
               policies = 0L, documents = 0L, state = "not found",
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  out[order(match(out$state, c("live", "provisioned", "not found")), -out$policies), ]
}

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
