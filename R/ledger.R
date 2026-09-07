# The decision ledger: append-only, hash-chained, human-attributed.
#
# This is the "Prove" step, and it is the only part of the instrument that an
# auditor will actually examine. Three properties are enforced here rather
# than left to convention, because each one is what a reviewer will try to
# break:
#
#   * Append-only. There is no update and no delete. A mistake is corrected by
#     appending a `correction.recorded` event that points at the event it
#     supersedes; the original stays readable forever. History is never
#     rewritten, so "what did the operator know at the time" remains
#     answerable after the fact.
#   * Hash-chained. Every event carries the hash of the one before it, so any
#     alteration of an earlier row invalidates every row after it and
#     ledger_verify() localises the break. This is what makes AU-9 evidence
#     rather than an assertion.
#   * Consequential acts are human. A disposition or a gate answer written
#     with actor_kind = "machine", or with no rationale, is REJECTED at write
#     time. The doctrine "the model may assist, the framework governs, a
#     person decides" is a schema constraint here, not a design principle in
#     a document — which is the difference between a claim and a control.
#
# Usage:
#   Rscript -e 'source("R/ledger.R"); ledger_init(); print(ledger_verify())'

suppressPackageStartupMessages({
  library(DBI); library(duckdb); library(digest); library(jsonlite)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  source(file.path(d, "R", "fetch_common.R"))
})

ledger_path <- function()
  Sys.getenv("KB_LEDGER", file.path(kb_root(), "data", "ledger.duckdb"))

# Events that change the state of a case in a way a regulator would care
# about. These require a human actor and a recorded rationale.
CONSEQUENTIAL <- c("operator.confirmed", "gate.answered", "disposition.recorded",
                   "notice.issued", "correction.recorded")

LEDGER_COLS <- c("seq", "event_id", "case_id", "occurred_at", "occurred_at_iso",
                 "actor", "actor_kind", "event_type", "gate_id", "rationale",
                 "authority", "framework_version", "payload",
                 "supersedes", "prev_hash", "hash")

ledger_con <- function(read_only = FALSE) {
  p <- ledger_path()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  dbConnect(duckdb(), p, read_only = read_only, array = "matrix")
}

ledger_init <- function(reset = FALSE) {
  con <- ledger_con(); on.exit(dbDisconnect(con, shutdown = TRUE))
  if (reset) dbExecute(con, "DROP TABLE IF EXISTS ledger")
  dbExecute(con, "CREATE TABLE IF NOT EXISTS ledger (
      seq BIGINT PRIMARY KEY, event_id VARCHAR, case_id VARCHAR,
      occurred_at TIMESTAMP, occurred_at_iso VARCHAR,
      actor VARCHAR, actor_kind VARCHAR,
      event_type VARCHAR, gate_id VARCHAR, rationale VARCHAR,
      authority VARCHAR, framework_version VARCHAR, payload VARCHAR,
      supersedes VARCHAR, prev_hash VARCHAR, hash VARCHAR)")
  invisible(TRUE)
}

#' Canonical serialisation of an event for hashing. Field order is fixed and
#' explicit: if it depended on column order or list order, a future schema
#' change would silently invalidate every historical hash.
#'
#' The timestamp is hashed from `occurred_at_iso`, the exact string stored on
#' the row, never re-derived from the TIMESTAMP column. Formatting a parsed
#' timestamp is not round-trip stable — DuckDB returns microsecond precision
#' in UTC while R formats in the session timezone — so every verification
#' failed on an untouched ledger. A chain that cries tampering when nothing
#' was tampered with is worse than no chain: it trains the reviewer to ignore
#' it. NA and NULL both serialise to "", so a field gaining a value later
#' cannot silently collide with one that never had it.
event_digest <- function(e, prev_hash) {
  nb <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "" else as.character(x)
  paste(prev_hash, e$event_id, e$case_id, nb(e$occurred_at_iso),
        e$actor, e$actor_kind, e$event_type, nb(e$gate_id),
        nb(e$rationale), nb(e$authority), nb(e$framework_version),
        e$payload %||% "", e$supersedes %||% "", sep = "") |>
    digest(algo = "sha256", serialize = FALSE)
}

#' Append one event. This is the only write path.
ledger_append <- function(case_id, event_type, actor, actor_kind = "human",
                          rationale = NULL, gate_id = NULL, authority = NULL,
                          framework_version = NULL, payload = list(),
                          supersedes = NULL, occurred_at = Sys.time(),
                          con = NULL) {
  if (event_type %in% CONSEQUENTIAL) {
    if (!identical(actor_kind, "human"))
      stop("refusing to write ", event_type, " with actor_kind='", actor_kind,
           "': consequential acts require a human actor", call. = FALSE)
    if (is.null(rationale) || !nzchar(trimws(rationale)))
      stop("refusing to write ", event_type,
           " with no rationale: a consequential act must record why",
           call. = FALSE)
  }
  own <- is.null(con)
  if (own) { con <- ledger_con(); on.exit(dbDisconnect(con, shutdown = TRUE)) }

  last <- dbGetQuery(con, "SELECT seq, hash FROM ledger ORDER BY seq DESC LIMIT 1")
  seq <- if (nrow(last)) last$seq[1] + 1L else 1L
  prev <- if (nrow(last)) last$hash[1] else ""

  ts <- as.POSIXct(occurred_at)
  e <- list(
    seq = seq, event_id = uuid::UUIDgenerate(), case_id = case_id,
    occurred_at = ts,
    occurred_at_iso = format(ts, "%Y-%m-%dT%H:%M:%OS3", tz = "UTC"),
    actor = actor, actor_kind = actor_kind,
    event_type = event_type, gate_id = gate_id %||% NA_character_,
    rationale = rationale %||% NA_character_, authority = authority %||% NA_character_,
    framework_version = framework_version %||% NA_character_,
    payload = if (length(payload)) toJSON(payload, auto_unbox = TRUE) else "{}",
    supersedes = supersedes %||% NA_character_, prev_hash = prev)
  e$payload <- as.character(e$payload)
  e$hash <- event_digest(e, prev)

  dbAppendTable(con, "ledger", as.data.frame(e, stringsAsFactors = FALSE))
  invisible(e)
}

ledger_events <- function(case_id = NULL, con = NULL) {
  own <- is.null(con)
  if (own) { con <- ledger_con(read_only = TRUE); on.exit(dbDisconnect(con, shutdown = TRUE)) }
  q <- "SELECT * FROM ledger"
  out <- if (is.null(case_id)) dbGetQuery(con, paste(q, "ORDER BY seq"))
         else dbGetQuery(con, paste(q, "WHERE case_id = ? ORDER BY seq"),
                         params = list(case_id))
  out
}

#' Walk the chain and report the first break, if any.
#'
#' Returns the sequence number where the chain fails rather than a bare
#' TRUE/FALSE, because "the ledger is broken" is not actionable and "row 412
#' does not match its recomputed hash" is.
ledger_verify <- function(con = NULL) {
  ev <- ledger_events(con = con)
  if (!nrow(ev)) return(list(ok = TRUE, events = 0L, broken_at = NA_integer_,
                             reason = "empty ledger"))
  prev <- ""
  for (i in seq_len(nrow(ev))) {
    e <- as.list(ev[i, ])
    if (!identical(e$prev_hash, prev))
      return(list(ok = FALSE, events = nrow(ev), broken_at = e$seq,
                  reason = "prev_hash does not match the preceding event"))
    if (!identical(event_digest(e, prev), e$hash))
      return(list(ok = FALSE, events = nrow(ev), broken_at = e$seq,
                  reason = "recomputed hash does not match the stored hash"))
    prev <- e$hash
  }
  list(ok = TRUE, events = nrow(ev), broken_at = NA_integer_,
       reason = "chain intact")
}

#' The events that still stand: a correction supersedes an earlier event, and
#' the superseded one remains in the ledger but is no longer operative.
ledger_current <- function(case_id, con = NULL) {
  ev <- ledger_events(case_id, con)
  if (!nrow(ev)) return(ev)
  superseded <- ev$supersedes[!is.na(ev$supersedes)]
  ev[!(ev$event_id %in% superseded), , drop = FALSE]
}
