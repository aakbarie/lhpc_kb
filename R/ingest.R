# PDF -> page-anchored chunks -> DuckDB, across every plan's corpus.
#
# Adapted from ../knowledge_base/policy_search/R/ingest.R. The page mapping,
# the swap-on-write build, and the incremental hash manifest are that file's
# design and are carried over deliberately; what changes for a multi-plan
# corpus is identity:
#
#   * every chunk carries `plan_id` and `line_of_business`, so results can be
#     faceted by plan and a citation can say WHOSE policy it is;
#   * a document's `origin` is "<plan_id>/<filename>", not the filename —
#     nine plans publish something called "Provider Manual", and keying on the
#     bare filename would let one plan's re-ingest delete another's chunks;
#   * the embedded context names the plan (see chunk_document()).
#
# Usage:
#   Rscript -e 'source("R/ingest.R"); ingest_all()'                  # incremental
#   Rscript -e 'source("R/ingest.R"); ingest_all(rebuild = TRUE)'    # from scratch
#   Rscript -e 'source("R/ingest.R"); ingest_all(plan_ids = "sfhp")' # one plan

suppressPackageStartupMessages({
  library(ragnar)
  library(pdftools)
  library(jsonlite)
  library(digest)
  library(dplyr)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "registry.R", "config.R"))
    source(file.path(d, "R", f))
})

#' Columns carried alongside every chunk. Declared once, because
#' ragnar_store_create() needs a prototype row and every insert must match it
#' exactly — drifting between the two is the classic way this breaks.
EXTRA_COLS <- function() data.frame(
  plan_id          = NA_character_,
  line_of_business = NA_character_,
  policy_number    = NA_character_,
  title            = NA_character_,
  department       = NA_character_,
  status           = NA_character_,
  date_published   = NA_character_,
  doc_kind         = NA_character_,
  filename         = NA_character_,
  page             = NA_integer_,
  page_count       = NA_integer_,
  stringsAsFactors = FALSE
)

#' Per-page text plus the offset at which each page starts in the joined
#' document. Returned together because they must be computed from the same
#' vector — recomputing either separately is how the mapping silently drifts
#' by a page, sending every citation to the wrong page while looking right.
pdf_pages_and_offsets <- function(path) {
  pages <- tryCatch(pdf_text(path), error = function(e) character())
  if (length(pages) == 0) return(NULL)
  list(pages  = pages,
       starts = cumsum(c(1, head(nchar(pages), -1) + 2)),   # "\n\n" between pages
       text   = paste(pages, collapse = "\n\n"))
}

#' Where a catalog row's PDF lives. Per-plan directories, so filenames only
#' have to be unique within a plan.
doc_path <- function(meta, cfg) file.path(cfg$pdf_dir, meta$plan_id, meta$filename)

#' Stable identity for a document across runs: unique across plans, and the
#' key both the hash manifest and ragnar's `origin` use.
doc_origin <- function(meta) file.path(meta$plan_id, meta$filename)

#' One PDF -> chunks carrying page numbers and catalog metadata.
#' NULL for a document with no extractable text (scanned; no OCR pass here).
chunk_document <- function(meta, cfg, plan_name = NULL) {
  path <- doc_path(meta, cfg)
  if (!file.exists(path)) return(NULL)

  pg <- pdf_pages_and_offsets(path)
  if (is.null(pg) || !nzchar(trimws(pg$text))) return(NULL)

  md <- MarkdownDocument(pg$text, origin = doc_origin(meta))
  ch <- markdown_chunk(md,
                       target_size    = as.integer(cfg$chunk_size),
                       target_overlap = cfg$chunk_overlap)
  if (nrow(ch) == 0) return(NULL)

  # Put the document's identity INSIDE the embedded text.
  #
  # ragnar embeds `context + "\n" + text`, and markdown_chunk fills `context`
  # with heading context only — so the policy number and title, the most
  # compressed statement of what a document is about, are invisible to
  # retrieval. policy_search found that this is what makes a single-plan
  # corpus return topical siblings ("Claims Processing" for an overpayment
  # question), and fixed it by prefixing number + title.
  #
  # Across 18 plans the same mechanism has a second job. Every plan writes
  # about the same Medi-Cal obligations in near-identical language, so
  # "UM01 Authorization Review" alone does not say whose rule it is; without
  # the plan in the embedded text, a question about Kern's timeframes can be
  # answered, fluently and with a citation, from Partnership's policy. Naming
  # the plan is the cheapest available defence, and it is measurable — turn it
  # off with KB_PLAN_IN_CONTEXT=0 and re-run the cross-plan eval.
  if (isTRUE(cfg$context_prefix)) {
    head <- trimws(paste(meta$policy_number, meta$title))
    if (isTRUE(cfg$plan_in_context) && !is.null(plan_name))
      head <- trimws(paste0(plan_name, " · ", head))
    ctx <- ch$context
    ctx[is.na(ctx)] <- ""
    ch$context <- paste0(head, "\n", ctx)
  }

  ch$page             <- as.integer(findInterval(ch$start, pg$starts))
  ch$page_count       <- length(pg$pages)
  ch$plan_id          <- meta$plan_id
  ch$line_of_business <- meta$line_of_business
  ch$policy_number    <- meta$policy_number
  ch$title            <- meta$title
  ch$department       <- meta$department
  ch$status           <- meta$status
  ch$date_published   <- meta$date_published
  ch$doc_kind         <- meta$doc_kind
  ch$filename         <- meta$filename
  ch
}

# --- ingest bookkeeping ------------------------------------------------------
# A sidecar manifest of "<plan_id>/<filename>" -> content hash, so re-running
# after adding ten policies costs ten policies' worth of embedding rather than
# the whole corpus.

#' Connect to an existing store for writing, with its schema repaired.
#'
#' ragnar 0.2.1's `ragnar_store_connect()` leaves `@schema` NULL, and
#' `ragnar_store_update()` needs it — it derives its join columns from
#' `select(store@schema, -doc_id, -embedding)`, so an incremental run dies
#' with "no applicable method for 'select' applied to an object of class
#' NULL".
#'
#' The prototype is recoverable from the store's own embeddings table, minus
#' `chunk_id`: a freshly created store's schema is
#' `doc_id, start, end, context, <extra_cols>, embedding`, and chunk_id is
#' assigned by the store rather than supplied by the caller. Leaving it in
#' puts it among the join columns, which fails differently ("Join columns in
#' `x` must be present in the data").
#'
#' Without this the only way to add a document is a full rebuild — ~2 hours
#' for this corpus, for ten new files.
connect_for_update <- function(path) {
  store <- ragnar_store_connect(path, read_only = FALSE)
  if (is.null(store@schema)) {
    proto <- dplyr::collect(head(dplyr::tbl(store@con, "embeddings"), 0))
    store@schema <- proto[, setdiff(names(proto), "chunk_id"), drop = FALSE]
  }
  store
}

hash_path <- function(cfg) file.path(dirname(cfg$store_path), "ingested.json")

read_hashes <- function(cfg) {
  p <- hash_path(cfg)
  if (!file.exists(p)) return(list())
  as.list(fromJSON(p))
}

write_hashes <- function(h, cfg) write_json(h, hash_path(cfg), auto_unbox = TRUE)

# --- main --------------------------------------------------------------------

#' Build or update the DuckDB store from the unified catalog.
#'
#' Always builds to a temp path and renames into place — incremental runs
#' included. DuckDB allows many readers or one writer, so opening the live
#' store read-write fails outright whenever anything else has it open (a
#' running app, or an RStudio session someone left connected). Ingest is a
#' background job; it must not be able to fail because someone is searching.
ingest_all <- function(cfg = kb_config(), rebuild = FALSE, limit = NULL,
                       plan_ids = NULL) {
  catalog <- read_catalog()
  if (nrow(catalog) == 0) stop("No catalog. Run fetch_all() first.")
  if (!is.null(plan_ids)) catalog <- catalog[catalog$plan_id %in% plan_ids, , drop = FALSE]
  if (!is.null(limit)) catalog <- head(catalog, limit)
  if (nrow(catalog) == 0) stop("No catalog rows for the requested plans.")

  reg <- kb_registry()
  plan_name <- vapply(reg, function(p) p$name %||% p$plan_id, "")

  embed  <- kb_embedder(cfg)
  hashes <- if (rebuild) list() else read_hashes(cfg)

  catalog$origin <- file.path(catalog$plan_id, catalog$filename)
  catalog$hash   <- NA_character_

  # Which documents actually need work?
  todo <- vapply(seq_len(nrow(catalog)), function(i) {
    p <- doc_path(catalog[i, ], cfg)
    if (!file.exists(p)) return(FALSE)
    h <- digest(file = p, algo = "md5")
    catalog$hash[i] <<- h
    !identical(hashes[[catalog$origin[i]]], h)
  }, logical(1))

  fresh <- rebuild || !file.exists(cfg$store_path)
  if (!any(todo) && !fresh) {
    message("nothing to do — every document in the catalog is already indexed")
    return(invisible(0L))
  }

  target <- paste0(cfg$store_path, ".building")
  unlink(c(target, paste0(target, ".wal")))

  if (fresh) {
    store <- ragnar_store_create(
      target, embed = embed, extra_cols = EXTRA_COLS(),
      name = "policies", title = "LHPC policy knowledge base", overwrite = TRUE)
    work <- which(file.exists(vapply(seq_len(nrow(catalog)),
                                     function(i) doc_path(catalog[i, ], cfg), "")))
  } else {
    # Copy, then extend the copy. Reading the live file needs no lock.
    if (!file.copy(cfg$store_path, target, overwrite = TRUE))
      stop("could not copy the existing store to ", target, call. = FALSE)
    store <- connect_for_update(target)
    work <- which(todo)
  }

  message(sprintf("indexing %d document(s) across %d plan(s) [%s]",
                  length(work), length(unique(catalog$plan_id[work])),
                  if (fresh) "full build" else "incremental"))

  total <- 0L; skipped <- character(); done <- 0L; halted <- NULL
  for (i in work) {
    meta <- catalog[i, ]
    ch <- tryCatch(chunk_document(meta, cfg, plan_name[[meta$plan_id]]),
                   error = function(e) {
                     warning("chunking failed for ", meta$origin, ": ",
                             conditionMessage(e), call. = FALSE)
                     NULL
                   })
    done <- done + 1L
    if (is.null(ch)) { skipped <- c(skipped, meta$origin); next }

    # A failure here is almost always the embedding endpoint (a rate limit
    # that outlived its retries, or a network drop), and it must not throw
    # away the documents already embedded — at ~55 chunks/document that is
    # hours of work. Stop the loop, then fall through to index and publish
    # what IS done; the hash manifest records exactly those documents, so a
    # plain `ingest_all()` re-run picks up where this one stopped.
    ok <- tryCatch({
      # update, not insert: re-ingesting a changed PDF must replace its old
      # chunks rather than leave both editions retrievable.
      ragnar_store_update(store, ch); TRUE
    }, error = function(e) { halted <<- conditionMessage(e); FALSE })
    if (!ok) break

    total <- total + nrow(ch)
    hashes[[meta$origin]] <- meta$hash
    if (done %% 25 == 0)
      message(sprintf("  %d/%d documents, %d chunks", done, length(work), total))
  }

  if (!is.null(halted))
    message("\nHALTED after ", done - 1, "/", length(work), " documents: ", halted,
            "\n  indexing and publishing what is complete — re-run ingest_all() to continue")

  message("building indexes (vss + fts)…")
  ragnar_store_build_index(store, type = "vss")
  ragnar_store_build_index(store, type = "fts")
  rm(store); gc(verbose = FALSE)

  unlink(paste0(target, ".wal"))
  if (!file.rename(target, cfg$store_path))
    stop("index built at ", target, " but could not replace ", cfg$store_path,
         call. = FALSE)
  write_hashes(hashes, cfg)

  if (length(skipped))
    message(sprintf("\nWARNING: %d document(s) had no extractable text (scanned; no OCR pass here): %s",
                    length(skipped), paste(head(skipped, 5), collapse = ", ")))
  message("done: ", total, " chunks indexed into ", cfg$store_path)
  if (!is.null(halted))
    warning("ingest stopped early (", halted, ") — re-run ingest_all() to continue",
            call. = FALSE)
  invisible(total)
}
