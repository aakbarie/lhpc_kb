# Tier 2 fetcher: static site crawl, driven entirely by the registry.
#
# A plan's `site:` block names listing pages (`seeds`) and/or document
# sitemaps (`doc_sitemaps`); links matching `doc_url_patterns` (fixed
# substrings) become catalog rows and are downloaded through the same guarded
# path as the API tiers. No per-plan code — the survey lives in the YAML.
#
# robots.txt is fetched once per host and honored (the `*` group). Plans
# whose CMS 403s non-browser user agents carry `user_agent: browser` in the
# registry; their document CDNs (Cloudinary, DAM paths) are open either way.

suppressPackageStartupMessages(library(xml2))

# --- robots.txt --------------------------------------------------------------

.robots_cache <- new.env(parent = emptyenv())

#' Disallow patterns of the `*` group, cached per origin. Unreachable or
#' absent robots.txt means no restrictions — the standard interpretation.
robots_disallows <- function(origin, ua = UA_DEFAULT) {
  if (!is.null(.robots_cache[[origin]])) return(.robots_cache[[origin]])
  txt <- tryCatch(
    kb_request(paste0(origin, "/robots.txt"), ua) |> req_perform() |> resp_body_string(),
    error = function(e) "")
  .robots_cache[[origin]] <- parse_robots(txt)
  .robots_cache[[origin]]
}

parse_robots <- function(txt) {
  lines <- trimws(sub("#.*$", "", strsplit(txt, "\r?\n")[[1]]))
  dis <- character(); agents <- character(); prev_was_agent <- FALSE
  for (ln in lines[nzchar(lines)]) {
    if (grepl("^user-agent\\s*:", ln, ignore.case = TRUE)) {
      if (!prev_was_agent) agents <- character()      # a rule ended the group
      agents <- c(agents, trimws(sub("^user-agent\\s*:", "", ln, ignore.case = TRUE)))
      prev_was_agent <- TRUE
    } else {
      prev_was_agent <- FALSE
      if (grepl("^disallow\\s*:", ln, ignore.case = TRUE) && "*" %in% agents) {
        rule <- trimws(sub("^disallow\\s*:", "", ln, ignore.case = TRUE))
        if (nzchar(rule)) dis <- c(dis, rule)
      }
    }
  }
  dis
}

#' Does the `*` group allow this URL? Robots patterns support `*` wildcards
#' and a `$` end anchor; everything else matches as a path prefix.
robots_allowed <- function(url, disallows) {
  path <- sub("^https?://[^/]+", "", url)
  if (!nzchar(path)) path <- "/"
  for (d in disallows) {
    anchored <- grepl("\\$$", d)
    if (anchored) d <- sub("\\$$", "", d)
    re <- gsub("([][{}().^$+?|\\\\])", "\\\\\\1", d, perl = TRUE)  # escape specials
    re <- gsub("*", ".*", re, fixed = TRUE)                        # robots * -> .*
    re <- paste0("^", re, if (anchored) "$" else "")
    if (grepl(re, path, perl = TRUE)) return(FALSE)
  }
  TRUE
}

# --- discovery ---------------------------------------------------------------

#' All links on one listing page that match any of the plan's document URL
#' patterns (fixed substrings), with their anchor text.
extract_doc_links <- function(page_url, patterns, ua, delay) {
  bytes <- tryCatch(
    kb_request(page_url, ua, delay) |> req_perform() |> resp_body_raw(),
    error = function(e) {
      warning("seed failed: ", page_url, " (", conditionMessage(e), ")", call. = FALSE)
      return(NULL)
    })
  if (is.null(bytes)) return(NULL)
  # A seed can BE the document (SCFHP's manual URL serves the PDF directly).
  if (length(bytes) >= 4 && identical(rawToChar(bytes[1:4]), "%PDF"))
    return(data.frame(url = page_url, text = "", seed = page_url,
                      stringsAsFactors = FALSE))
  doc  <- read_html(rawToChar(bytes))
  a    <- xml_find_all(doc, ".//a[@href]")
  url  <- xml2::url_absolute(xml_attr(a, "href"), page_url)
  text <- trimws(gsub("\\s+", " ", xml_text(a)))
  keep <- Reduce(`|`, lapply(patterns, function(p) grepl(p, url, fixed = TRUE)))
  if (!any(keep)) {
    warning("seed matched nothing: ", page_url, call. = FALSE)
    return(NULL)
  }
  data.frame(url = sub("#.*$", "", url[keep]), text = text[keep],
             seed = page_url, stringsAsFactors = FALSE)
}

#' Document URLs from an XML sitemap, filtered to `include` substrings.
sitemap_doc_urls <- function(sm, ua, delay) {
  x <- tryCatch(
    kb_request(sm$url, ua, delay) |> req_perform() |> resp_body_string() |> read_xml(),
    error = function(e) {
      warning("doc sitemap failed: ", sm$url, call. = FALSE)
      return(NULL)
    })
  if (is.null(x)) return(NULL)
  locs <- xml_text(xml_find_all(x, "//*[local-name()='loc']"))
  inc  <- unlist(sm$include)
  if (length(inc))
    locs <- locs[Reduce(`|`, lapply(inc, function(p) grepl(p, locs, fixed = TRUE)))]
  if (!length(locs)) return(NULL)
  data.frame(url = locs, text = "", seed = sm$url, stringsAsFactors = FALSE)
}

# --- classification ----------------------------------------------------------

#' Document kind from the link text and URL. Order matters: "formulary"
#' contains "form", so it is tested first.
classify_kind <- function(url, text) {
  s <- tolower(paste(text, utils::URLdecode(url)))
  if (grepl("formulary|drug list|\\bpdl\\b", s))                       "formulary"
  else if (grepl("handbook|evidence of coverage|\\beoc\\b", s))        "member"
  else if (grepl("manual|ops guide|operations guide|resource guide", s)) "manual"
  else if (grepl("newsletter|bulletin|\\bmemos?\\b|\\balerts?\\b|\\bnotices?\\b", s)) "notice"
  else if (grepl("\\bforms?\\b|application|attestation|checklist|template", s)) "form"
  else "policy"
}

#' Department facet from URL fragments — coarse, but enough to drive the
#' same facets the API tiers get for free.
guess_department <- function(url, seed) {
  s <- tolower(paste(url, seed))
  if      (grepl("utilization|um-|um_|/um/", s))   "Utilization Management"
  else if (grepl("pharmac|/rx", s))                "Pharmacy"
  else if (grepl("credential", s))                 "Credentialing"
  else if (grepl("quality|qm-|qm_", s))            "Quality"
  else if (grepl("claim", s))                      "Claims"
  else if (grepl("clinical|guideline", s))         "Clinical"
  # a "manual" signal must come from the document itself — listing pages are
  # routinely named ".../forms-manuals" and would stamp every row on them
  else if (grepl("manual", tolower(url)))          "Provider Manual"
  else                                             "Provider Resources"
}

#' Anchor text when it names the document; otherwise a name derived from the
#' file itself. "Download" is a label, not a title. Trailing link decoration
#' ("»", "›") is markup, not name.
doc_title <- function(text, url) {
  text <- trimws(gsub("[»›>]+\\s*$", "", text))
  generic <- grepl("^(|download|pdf|here|view|click here|english|spanish|link)$",
                   tolower(text))
  if (!generic && nchar(text) >= 4) return(text)
  base <- utils::URLdecode(basename(sub("\\?.*$", "", url)))
  gsub("_|-", " ", sub("\\.[A-Za-z0-9]+$", "", base))
}

#' Leading policy code, when the title carries one: CO-01, UM01, QM38,
#' HS-001, PS-CR03. Titles starting with a year or a word don't match.
SITE_CODE_RE <- "^([A-Z]{1,6}(-[A-Z]{1,4})?-?[0-9]{1,4}[A-Za-z]?)([[:space:]:.,–-]|$)"

extract_code <- function(title) {
  if (grepl(SITE_CODE_RE, title)) sub(paste0(SITE_CODE_RE, ".*$"), "\\1", title) else ""
}

# --- catalog + fetch ---------------------------------------------------------

site_catalog <- function(plan) {
  ua       <- kb_ua(plan)
  delay    <- plan$crawl_delay %||% 1
  patterns <- unlist(plan$site$doc_url_patterns)

  message("[", plan$plan_id, "] crawling ", length(plan$site$seeds %||% c()),
          " seed(s), ", length(plan$site$doc_sitemaps %||% c()), " doc sitemap(s)")

  links <- do.call(rbind, c(
    lapply(unlist(plan$site$seeds), function(s) {
      if (!robots_allowed(s, robots_disallows(sub("^(https?://[^/]+).*$", "\\1", s), ua))) {
        warning("robots disallows seed, skipping: ", s, call. = FALSE)
        return(NULL)
      }
      extract_doc_links(s, patterns, ua, delay)
    }),
    lapply(plan$site$doc_sitemaps %||% list(), sitemap_doc_urls, ua = ua, delay = delay)
  ))
  if (is.null(links) || nrow(links) == 0)
    stop("[", plan$plan_id, "] no documents discovered — seeds or patterns are wrong")

  links <- links[!duplicated(links$url), , drop = FALSE]
  ok <- vapply(links$url, function(u)
    robots_allowed(u, robots_disallows(sub("^(https?://[^/]+).*$", "\\1", u), ua)), TRUE)
  if (any(!ok)) message("  robots.txt excludes ", sum(!ok), " document(s)")
  links <- links[ok, , drop = FALSE]

  lob <- paste(plan$lines_of_business %||% "medi-cal", collapse = ",")
  rows <- lapply(seq_len(nrow(links)), function(i) {
    l <- links[i, ]
    title <- doc_title(l$text, l$url)
    catalog_row(
      plan_id          = plan$plan_id,
      line_of_business = lob,
      doc_id           = sub("^https?://", "", l$url),
      policy_number    = extract_code(title),
      title            = title,
      department       = guess_department(l$url, l$seed),
      doc_kind         = classify_kind(l$url, paste(l$text, title)),
      source_url       = l$url,
      filename         = paste0(safe_name(title), ".pdf")
    )
  })
  cat <- dedupe_filenames(do.call(rbind, rows))

  scope <- unlist(plan$scope)
  if (length(scope)) {
    drop <- !cat$doc_kind %in% scope
    if (any(drop)) message("  scope keeps ", sum(!drop), "/", nrow(cat),
                           " (dropping ", paste(unique(cat$doc_kind[drop]), collapse = ", "), ")")
    cat <- cat[!drop, , drop = FALSE]
  }

  # A site plan can also carry a browser-harvested manifest (IEHP's manual
  # chapters live behind SAI360 but were verified on the open DAM). Manifest
  # rows join the crawl; crawl rows win a URL collision.
  if (file.exists(manifest_path(plan))) {
    extra <- manifest_catalog(plan)
    extra <- extra[!extra$source_url %in% cat$source_url, , drop = FALSE]
    message("  + ", nrow(extra), " manifest document(s)")
    cat <- dedupe_filenames(rbind(cat, extra))
  }
  cat
}

site_fetch <- function(plan, limit = NULL, refresh = FALSE) {
  cat <- site_catalog(plan)
  if (!is.null(limit)) cat <- head(cat, limit)
  fetch_documents(plan, cat, refresh)
}
