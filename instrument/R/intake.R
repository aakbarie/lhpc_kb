# Case intake: recording the covariates the analysis will need.
#
# WHY THIS FILE EXISTS. The outcome analytics estimate the effect of the
# framework assistant on timeliness, and that estimate is only defensible if
# the confounders in the DAG were measured. They were not: the ledger carried
# the treatment and the outcome and none of complexity, backlog, tenure or
# social barriers, so the analysis ran on simulated data and failed outright
# on real cases. An instrument that produces an audit record but not the
# evidence to evaluate itself is half-built.
#
# WHY AT INTAKE, SPECIFICALLY. A covariate recorded after the operator
# consults the assistant may have been affected by it — that is
# post-treatment bias, and adjusting for such a variable damages the estimate
# rather than protecting it. So these are captured when the case is opened,
# before any assistance is possible, and the ledger's append-only guarantee
# is what makes "before" verifiable rather than asserted.
#
# WHAT IS AND IS NOT MEASURABLE HERE. Backlog and tenure the instrument knows.
# Expedited status the case carries. Complexity is assessed by the operator at
# intake, which is subjective and is labelled as such. Social barriers need a
# member-record connector this deployment does not have, so they are recorded
# as NA rather than guessed — and analysis_readiness() then reports the
# confounder as unmeasured instead of letting an estimate proceed as if it
# had been controlled for.

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  inst <- file.path(d, "instrument"); if (!dir.exists(file.path(inst, "R"))) inst <- d
  for (f in c("fetch_common.R", "config.R")) source(file.path(d, "R", f))
  for (f in c("passport.R", "ledger.R")) source(file.path(inst, "R", f))
})

# The covariates the DAG's adjustment set names. Declared here so the
# instrument and the analysis agree on the vocabulary; a rename in one place
# without the other is caught by analysis_readiness().
INTAKE_COVARIATES <- c("complexity", "expedited", "backlog", "tenure", "sdoh")

#' Queue depth when a case is opened: how many cases are already open.
#'
#' The instrument can compute this and nobody else can reconstruct it later —
#' by the time anyone asks, the queue has moved. That is the general argument
#' for capturing confounders at intake rather than mining them afterwards.
current_backlog <- function(at = Sys.time(), con = NULL) {
  ev <- tryCatch(ledger_events(con = con), error = function(e) NULL)
  if (is.null(ev) || !nrow(ev)) return(0L)
  opened <- unique(ev$case_id[ev$event_type == "case.created" &
                              ev$occurred_at <= at])
  closed <- unique(ev$case_id[ev$event_type == "disposition.recorded" &
                              ev$occurred_at <= at])
  length(setdiff(opened, closed))
}

#' Operator tenure in years, from the deployment's staff file.
#'
#' Alongside reporting lines, because both are org facts the instrument is
#' told rather than infers. Missing operator = NA, never a default: an
#' invented 3.0 would enter the propensity model as though it were known.
operator_tenure <- function(operator,
                            path = file.path(inst_root(), "instruments",
                                             "reporting-lines.yml")) {
  if (!file.exists(path)) return(NA_real_)
  y <- yaml::read_yaml(path)
  t <- y$tenure_years %||% list()
  v <- t[[operator %||% ""]]
  if (is.null(v)) NA_real_ else as.numeric(v)
}

#' Open a case, recording the covariates as they stand before any assistance.
#' `backlog` may be supplied when the caller already knows the queue depth —
#' seeding a history recomputes it per case otherwise, which rescans the whole
#' ledger every time and turns a demo into an O(n^2) wait.
record_intake <- function(case_id, operator, expedited = FALSE,
                          complexity = NA_real_, sdoh = NA_real_,
                          subject_ref = NULL, category = NULL, backlog = NULL,
                          simulated = FALSE,
                          occurred_at = Sys.time(), con = NULL) {
  cov <- list(
    complexity = complexity,          # operator-assessed, 1-10, subjective
    expedited  = as.integer(isTRUE(expedited)),
    backlog    = backlog %||% current_backlog(occurred_at, con = con),
    tenure     = operator_tenure(operator),
    sdoh       = sdoh,                # NA until a member-record connector exists
    category   = category %||% NA_character_,
    # Seeded cases are marked in the record itself, so a reader can always
    # tell which history is real. The flag travels with the event rather than
    # living in a filename someone can rename.
    simulated  = as.integer(isTRUE(simulated)),
    measured_at = "intake")
  ledger_append(case_id, "case.created", operator, actor_kind = "human",
                subject_ref = subject_ref, payload = cov,
                occurred_at = occurred_at, con = con)
}

#' Which DAG confounders are actually present in a case-level frame, and
#' whether an estimate is defensible at all.
#'
#' Returning a verdict rather than a warning is deliberate. A missing
#' confounder does not make an estimate slightly worse, it makes it an
#' estimate of something else; the analysis should decline rather than
#' produce a number with a footnote nobody reads.
analysis_readiness <- function(df, required = INTAKE_COVARIATES) {
  if (is.null(df) || !nrow(df))
    return(list(ready = FALSE, present = character(), missing = required,
                unmeasured = character(), n = 0L,
                message = "No cases recorded yet."))
  present <- intersect(required, names(df))
  absent <- setdiff(required, names(df))
  # A column that exists but is entirely NA is not measured either — the
  # more dangerous case, because it survives a `names()` check.
  unmeasured <- present[vapply(present, function(c) all(is.na(df[[c]])), TRUE)]
  usable <- setdiff(present, unmeasured)
  list(
    ready = length(absent) == 0 && length(unmeasured) == 0 && nrow(df) >= 30,
    present = usable, missing = absent, unmeasured = unmeasured, n = nrow(df),
    message = if (length(absent) || length(unmeasured))
      sprintf("Cannot estimate: %s not recorded at intake. An adjustment set with a missing confounder does not estimate the effect of the assistant.",
              paste(union(absent, unmeasured), collapse = ", "))
    else if (nrow(df) < 30)
      sprintf("Only %d cases with complete covariates. Too few to estimate.", nrow(df))
    else sprintf("%d cases with all confounders recorded at intake.", nrow(df)))
}
