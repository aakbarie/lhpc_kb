# Cases and gate clocks.
#
# The case is the unit of work, and a gate is an obligation with a clock. Gate
# status is computed from the ledger, never stored on the case — the ledger is
# the record, and a status column beside it would be a second source of truth
# that can disagree with the evidence.
#
# The demo cases here are SYNTHETIC. No real member data: names are invented,
# identifiers are obviously fake, and nothing derives from the corpus except
# the framework text, which is public policy. A pre-operational instrument
# demonstrated on real grievances would be a privacy incident, not a demo.

suppressPackageStartupMessages({ library(yaml) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "ledger.R", "passport.R")) source(file.path(d, "R", f))
})

#' Parse "5 calendar days" / "72 hours" into seconds.
#'
#' Some obligations have no duration — "at intake" must simply be done, and it
#' is not late at any particular moment. Those return NA and are treated as
#' clockless below rather than as a deadline of zero, which would show every
#' new case as instantly breached and make the breach signal worthless.
clock_seconds <- function(clock) {
  if (!grepl("\\d", clock)) return(NA_real_)
  n <- suppressWarnings(as.numeric(sub("^\\D*(\\d+).*$", "\\1", clock)))
  if (is.na(n)) return(NA_real_)
  if (grepl("hour", clock, ignore.case = TRUE)) n * 3600 else n * 86400
}

instrument_gates <- function(id = "grievance-timeliness") {
  spec <- instrument_spec(id)
  gates <- spec$framework$gates
  data.frame(
    gate_id = vapply(gates, `[[`, "", "id"),
    obligation = vapply(gates, `[[`, "", "obligation"),
    clock = vapply(gates, `[[`, "", "clock"),
    basis = vapply(gates, function(g) g$basis %||% "", ""),
    automatable = vapply(gates, function(g) g$automatable %||% "", ""),
    seconds = vapply(gates, function(g) clock_seconds(g$clock), 0),
    stringsAsFactors = FALSE)
}

SYNTHETIC_CASES <- function() data.frame(
  case_id  = c("GRV-2026-0412", "GRV-2026-0413", "GRV-2026-0414", "GRV-2026-0415"),
  member   = c("SYNTHETIC-M-88213", "SYNTHETIC-M-40117",
               "SYNTHETIC-M-73902", "SYNTHETIC-M-15588"),
  received = as.POSIXct(Sys.time()) -
             c(2 * 86400, 27 * 86400, 40 * 3600, 6 * 86400),
  expedited = c(FALSE, FALSE, TRUE, FALSE),
  # The authorization or determination being grieved. Independence is
  # evaluated against prior decisions on THIS subject, so a case with no
  # subject can only check the resolver's own history.
  subject_ref = c("AUTH-2026-55120", NA, "AUTH-2026-55984", NA),
  category = c("Access to care — specialist referral delay",
               "Billing — balance billed for covered service",
               "Pharmacy — urgent medication denial",
               "Transportation — NEMT no-show"),
  intake = c(
    "Member reports waiting six weeks for a dermatology referral after PCP submitted it.",
    "Member received a bill from a contracted facility for a service the plan covered.",
    "Member's pharmacy denied an urgent prescription; member reports worsening symptoms.",
    "Scheduled non-emergency transport did not arrive; member missed a dialysis appointment."),
  stringsAsFactors = FALSE)

#' Gate status for one case, derived from the framework and the ledger.
#'
#' `answered` comes from a gate.answered event, which by construction can only
#' have been written by a human with a rationale. So "met" here means a person
#' decided it and said why — not that a timer stopped.
case_gates <- function(case_id, received, expedited = FALSE,
                       id = "grievance-timeliness", now = Sys.time()) {
  g <- instrument_gates(id)
  g <- g[g$gate_id != if (expedited) "RESOLVE_STANDARD" else "RESOLVE_EXPEDITED", ]

  ev <- tryCatch(ledger_current(case_id), error = function(e) NULL)
  answered <- if (!is.null(ev) && nrow(ev))
    ev$gate_id[ev$event_type %in% c("gate.answered", "disposition.recorded")] else character()

  g$clockless <- is.na(g$seconds)
  g$due <- received + ifelse(g$clockless, 0, g$seconds)
  g$met <- g$gate_id %in% answered
  g$remaining <- ifelse(g$clockless, NA_real_,
                        as.numeric(difftime(g$due, now, units = "hours")))
  g$status <- ifelse(g$met, "met",
              ifelse(g$clockless, "open",
              ifelse(g$remaining < 0, "breached",
              ifelse(g$remaining < 24, "due soon", "open"))))
  # Clockless obligations sort last: a deadline the operator can miss should
  # sit above one they merely have to perform.
  g[order(g$clockless, g$due), , drop = FALSE]
}

#' The framework passage behind a gate, quoted from the corpus.
#'
#' The instrument must never present generated or paraphrased language as
#' policy, so this returns real retrieved text with its document and page, or
#' nothing at all. Silence is the correct output when the store is absent —
#' an empty framework panel is honest, a plausible paraphrase is not.
#' `terms` narrows the passage to the one governing THIS gate. Without it every
#' gate on a case shows the same first passage of the letter, which is true but
#' useless — an operator answering the acknowledgement gate needs the
#' acknowledgement language, and a panel that shows the same paragraph three
#' times reads as decoration rather than authority.
gate_authority_text <- function(basis, terms = character(),
                                cfg = tryCatch(kb_config(), error = function(e) NULL)) {
  if (is.null(cfg) || !file.exists(cfg$store_path) || !nzchar(basis)) return(NULL)
  con <- tryCatch(DBI::dbConnect(duckdb::duckdb(), cfg$store_path,
                                 read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  key <- paste("APL", gsub("[^0-9-]", "", basis))

  ask <- function(where, params) tryCatch(
    DBI::dbGetQuery(con, paste0(
      "SELECT plan_id, title, page, substr(text, 1, 700) AS passage
       FROM chunks WHERE policy_number = ? AND length(text) > 300 ", where,
      " ORDER BY page LIMIT 1"), params = params), error = function(e) NULL)

  if (length(terms)) {
    rx <- paste0("(?i)(", paste(terms, collapse = "|"), ")")
    out <- ask("AND regexp_matches(text, ?)", list(key, rx))
    if (!is.null(out) && nrow(out)) return(out)
  }
  out <- ask("", list(key))                     # fall back to the letter itself
  if (is.null(out) || !nrow(out)) return(NULL)
  out
}

#' Retrieval terms per gate, derived from the obligation rather than hardcoded,
#' so a new gate in the YAML gets sensible retrieval without a code change.
gate_terms <- function(gate_id, obligation) {
  base <- switch(gate_id,
    ACK = c("acknowledge", "acknowledgement", "receipt"),
    RESOLVE_STANDARD = c("thirty", "30 calendar days", "resolution", "resolve"),
    RESOLVE_EXPEDITED = c("expedited", "72 hours", "seventy-two"),
    CLASSIFY = c("exempt", "inquiry", "classif"),
    NULL)
  if (!is.null(base)) return(base)
  w <- unlist(strsplit(tolower(obligation), "[^a-z]+"))
  unique(w[nchar(w) > 5])
}
