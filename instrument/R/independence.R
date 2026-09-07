# Reviewer independence.
#
# The rule, in the plans' own words:
#
#   "resolution of a grievance or appeal has not participated in any prior
#    decisions related to the grievance or appeal and is not a subordinate of
#    any such individual"                      — Alliance 403-1160 p.15
#
#   "Appeal decisions requiring a medical necessity review will be made by an
#    appropriate practitioner who was not involved in the initial denial
#    decision and will not be determined by an Alliance staff member who is
#    subordinate to the reviewer"              — Alliance 200-9007 p.3
#
# Eight plans carry this language. It is almost always satisfied in practice
# and almost never evidenced, because the prior decision was made in a
# different system — a UM or care management platform — whose audit trail is
# built to answer "what happened to this member" rather than "did the person
# resolving this grievance decide the thing being grieved".
#
# So the instrument does two things. It accepts an attestation of who made the
# original determination, recorded as a ledger event with its source system,
# and it refuses a disposition written by anyone conflicted against those
# attestations. Independence stops being a policy assertion and becomes a
# precondition of the write.
#
# What this deliberately does NOT do: reach into the clinical system. The
# clinical record stays where it is. Only the fact of who decided, and when,
# is imported — the minimum necessary to evaluate the rule.

suppressPackageStartupMessages({ library(yaml) })

#' Reporting lines: person -> supervisor. Supplied per deployment because an
#' org chart is not something to infer. Absent file = no subordinate checks,
#' and independence_check() says so rather than silently passing.
reporting_lines <- function(path = file.path(inst_root(), "instruments",
                                             "reporting-lines.yml")) {
  if (!file.exists(path)) return(NULL)
  y <- yaml::read_yaml(path)
  unlist(y$reports_to %||% list())
}

#' Everyone above `person` in the reporting chain. Cycles are survivable — a
#' malformed org chart must not hang the write path.
supervisors_of <- function(person, lines = reporting_lines(), max_depth = 12) {
  if (is.null(lines) || !nzchar(person %||% "")) return(character())
  out <- character(); cur <- person
  for (i in seq_len(max_depth)) {
    sup <- unname(lines[cur])
    if (is.na(sup) || !nzchar(sup) || sup %in% out) break
    out <- c(out, sup); cur <- sup
  }
  out
}

#' Is `person` at or below `other` in the reporting chain?
is_subordinate_of <- function(person, other, lines = reporting_lines()) {
  identical(person, other) || other %in% supervisors_of(person, lines)
}

# Events that constitute a determination about the subject. An attested prior
# decision counts even though it happened in another system — that is the
# whole point.
DETERMINATION_EVENTS <- c("disposition.recorded", "gate.answered",
                          "prior_decision.attested")

#' Conflicts that would disqualify `actor` from resolving this case.
#'
#' Returns a data frame of conflicting prior determinations (empty = clear),
#' plus an attribute saying whether subordinate checking was possible. An
#' empty result with no org chart loaded is "not disqualified on the evidence
#' available", which is a weaker claim than "independent" and is reported as
#' such rather than being rounded up.
independence_conflicts <- function(case_id, actor, subject_ref = NULL, con = NULL) {
  own <- is.null(con)
  if (own) { con <- ledger_con(read_only = TRUE); on.exit(dbDisconnect(con, shutdown = TRUE)) }

  # What disqualifies is a determination about the thing being REVIEWED, not
  # the resolver's own steps within this grievance. Answering the
  # acknowledgement gate must not disqualify the same person from answering
  # the resolution gate — that would make a single-handed case impossible and
  # is not what the rule says.
  #
  # So conflicts are drawn from: decisions attested from another system
  # (the original denial), and determinations recorded on a DIFFERENT case
  # that shares this subject. A determination on this case by this actor is
  # the work itself.
  has_subject <- !is.null(subject_ref) && nzchar(subject_ref)
  ev <- dbGetQuery(con, sprintf(
    "SELECT seq, event_id, case_id, subject_ref, actor, actor_kind, event_type,
            occurred_at, payload
       FROM ledger
      WHERE (event_type = 'prior_decision.attested' %s)
        AND (case_id = ? %s)
      ORDER BY seq",
    if (has_subject) "OR (event_type IN ('disposition.recorded','gate.answered') AND case_id <> ?)" else "",
    if (has_subject) "OR subject_ref = ?" else ""),
    params = if (has_subject) list(case_id, case_id, subject_ref) else list(case_id))

  lines <- reporting_lines()
  if (!nrow(ev)) {
    out <- ev[0, , drop = FALSE]
  } else {
    conflicted <- vapply(seq_len(nrow(ev)), function(i)
      is_subordinate_of(actor, ev$actor[i], lines), TRUE)
    out <- ev[conflicted, , drop = FALSE]
    if (nrow(out)) out$why <- ifelse(out$actor == actor,
      "actor made this determination",
      paste0("actor reports (directly or indirectly) to ", out$actor))
  }
  attr(out, "org_chart_loaded") <- !is.null(lines)
  out
}

#' Human-readable verdict for the UI and for the record.
independence_check <- function(case_id, actor, subject_ref = NULL, con = NULL) {
  cf <- independence_conflicts(case_id, actor, subject_ref, con)
  list(
    clear = nrow(cf) == 0,
    conflicts = cf,
    org_chart_loaded = isTRUE(attr(cf, "org_chart_loaded")),
    statement = if (nrow(cf))
        paste0("Not independent: ", paste(unique(cf$why), collapse = "; "))
      else if (isTRUE(attr(cf, "org_chart_loaded")))
        "Independent: no prior determination by this actor or anyone they report to"
      else
        "No conflicting prior determination on record; reporting lines not configured, so subordinate status was not evaluated")
}
