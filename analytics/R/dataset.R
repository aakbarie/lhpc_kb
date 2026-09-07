# Case-level analysis frame.
#
# The instrument's ledger is an event log; causal analysis needs one row per
# case with a treatment, an outcome and covariates. This turns the first into
# the second, and nothing here writes back.
#
# THE SIMULATION NEVER TOUCHES THE LEDGER. A pre-operational instrument has no
# outcome history, so the demo needs one — but the ledger is an audit record,
# and writing invented cases into it would be corrupting evidence to make a
# chart. Simulated histories are returned as a plain data frame, carry a
# `simulated` column, and are labelled everywhere they surface.

suppressPackageStartupMessages({ library(DBI); library(duckdb) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "config.R")) source(file.path(d, "R", f))
  for (f in c("passport.R", "ledger.R")) source(file.path(d, "instrument", "R", f))
})

#' One row per case, from real ledger events.
#'
#' `assisted` is the treatment: did anyone consult the framework assistant on
#' this case before the determination. `on_time` is the outcome: was the
#' resolution gate answered before its deadline.
case_outcomes <- function() {
  ev <- tryCatch(ledger_events(), error = function(e) NULL)
  if (is.null(ev) || !nrow(ev)) return(NULL)

  # Built with vectorised operations rather than a per-case data.frame and
  # rbind. The loop version was quadratic — 700 cases took 2.8 s, and it grows
  # with the ledger, which is the one thing guaranteed to keep growing.
  created <- ev[ev$event_type == "case.created", , drop = FALSE]
  disp <- ev[ev$event_type == "disposition.recorded", , drop = FALSE]
  disp <- disp[!duplicated(disp$case_id), , drop = FALSE]
  asked <- unique(ev$case_id[ev$event_type == "assistance.requested"])

  opened <- stats::aggregate(occurred_at ~ case_id, data = ev, FUN = min)
  ids <- created$case_id
  if (!length(ids)) return(NULL)

  # One parse per case, but no data.frame churn: pull the fields out into
  # plain vectors and assemble once.
  fields <- c("complexity", "expedited", "backlog", "tenure", "sdoh")
  parsed <- lapply(created$payload, function(js)
    tryCatch(jsonlite::fromJSON(js), error = function(e) list()))
  grab <- function(k) vapply(parsed, function(p) {
    v <- p[[k]]
    if (is.null(v) || length(v) == 0 || identical(v, "NA")) NA_real_
    else suppressWarnings(as.numeric(v)[1])
  }, 0)

  m_open <- match(ids, opened$case_id)
  m_disp <- match(ids, disp$case_id)
  out <- data.frame(
    case_id = ids,
    simulated = grab("simulated") == 1,
    assisted = as.integer(ids %in% asked),
    resolved = !is.na(m_disp),
    hours_to_resolution = as.numeric(difftime(disp$occurred_at[m_disp],
                                              opened$occurred_at[m_open],
                                              units = "hours")),
    operator = created$actor,
    complexity = grab("complexity"), expedited = grab("expedited"),
    backlog = grab("backlog"), tenure = grab("tenure"), sdoh = grab("sdoh"),
    stringsAsFactors = FALSE)
  out$simulated[is.na(out$simulated)] <- FALSE
  add_rollout_panel(out, ev)
}

#' Recover the staggered-adoption panel the contract's DiD design needs.
#'
#' An operator's adoption date is the first time they consulted the
#' assistant; `post` marks cases they opened after it, `adopter` marks the
#' operators who ever did. Derived from the ledger rather than configured,
#' because adoption is a behaviour and the ledger is where it happened —
#' a roster of who was "given access" would measure intent instead.
add_rollout_panel <- function(df, ev) {
  if (is.null(df) || !nrow(df)) return(df)
  opened <- ev[ev$event_type == "case.created", c("case_id", "actor", "occurred_at")]
  used <- ev[ev$event_type == "assistance.requested", c("actor", "occurred_at")]
  first_use <- tapply(used$occurred_at, used$actor, min)

  m <- match(df$case_id, opened$case_id)
  df$opened_at <- opened$occurred_at[m]
  adopt <- first_use[df$operator]
  df$adopter <- as.integer(!is.na(adopt))

  # `post` is CALENDAR time against a common rollout date, not each
  # operator's own adoption. Defining it per-operator meant non-adopters
  # never had a post period, so post = 1 implied adopter = 1, the
  # interaction was collinear with post, and R dropped it — the estimate
  # failed with "subscript out of bounds" rather than returning something
  # wrong, which was lucky. A difference-in-differences needs both groups
  # observed on both sides of the same line; the rollout date is that line.
  rollout <- if (length(first_use)) stats::median(as.numeric(first_use)) else NA_real_
  df$post <- if (is.na(rollout)) 0L else
    as.integer(as.numeric(df$opened_at) >= rollout)
  attr(df, "rollout_date") <- if (is.na(rollout)) NA else
    as.POSIXct(rollout, origin = "1970-01-01")
  df
}

#' A simulated operational history, with a KNOWN true effect.
#'
#' Two properties make this worth showing rather than a random scatter:
#'
#'   * The confound is real and is the one this operation actually has.
#'     Analysts consult the assistant on the cases they find hard, and hard
#'     cases run late for reasons that have nothing to do with the assistant.
#'     So a naive comparison of assisted against unassisted cases will show
#'     assistance making things WORSE, which is the trap the matching exists
#'     to escape.
#'   * The true effect is fixed here (`true_effect`), so the estimate can be
#'     checked rather than admired. A demo that cannot be wrong teaches
#'     nothing.
#' The operator panel is what makes the DiD identified. Cases are
#' cross-sectional — each is seen once — so there is no before/after WITHIN a
#' case to difference. Operators are the panel: each handles cases in both
#' periods, some adopt the assistant when the instrument deploys and some do
#' not, and the difference of those differences is the estimate. Without this
#' structure the DiD is a two-group comparison wearing a panel's name.
simulate_history <- function(n = 900, true_effect = -9, seed = 42,
                             deploy_after = 0.5, n_operators = 30) {
  set.seed(seed)
  complexity <- round(pmin(pmax(rnorm(n, 5, 2), 1), 10), 1)   # 1..10
  expedited  <- rbinom(n, 1, 0.18)
  backlog    <- round(pmax(rnorm(n, 40, 14), 5))
  tenure     <- round(pmax(rnorm(n, 3.2, 1.8), 0.2), 1)       # operator years
  day        <- sample(seq_len(n))                            # arrival order
  operator   <- sprintf("op-%02d", sample(seq_len(n_operators), n, replace = TRUE))
  # Operator-level heterogeneity: some are simply faster, and a subset adopt
  # the assistant once it is available.
  op_effect  <- setNames(rnorm(n_operators, 0, 12), sprintf("op-%02d", seq_len(n_operators)))
  adopter    <- setNames(rbinom(n_operators, 1, 0.5), sprintf("op-%02d", seq_len(n_operators)))

  # Assistance is SOUGHT on hard cases, by less experienced staff, when the
  # queue is deep. That is the confound.
  post <- as.integer(day > deploy_after * n)
  p_assist <- plogis(-3.1 + 0.42 * complexity + 0.9 * expedited +
                     0.020 * backlog - 0.28 * tenure +
                     1.6 * post * adopter[operator])
  assisted <- rbinom(n, 1, p_assist)

  # Hours to resolution rises with complexity and backlog, falls with tenure.
  # Expedited cases are worked faster because their clock is 72 hours.
  base <- 130 + 26 * complexity + 0.85 * backlog - 9 * tenure - 58 * expedited +
          op_effect[operator]
  hours <- base + true_effect * assisted + rnorm(n, 0, 34)
  hours <- pmax(hours, 2)

  # Deadline: 72h expedited, 30 days standard.
  deadline <- ifelse(expedited == 1, 72, 720)

  data.frame(
    case_id = sprintf("SIM-%04d", seq_len(n)),
    simulated = TRUE,
    assisted = assisted,
    complexity = complexity,
    expedited = expedited,
    backlog = backlog,
    tenure = tenure,
    arrival = day,
    operator = operator,
    adopter = as.integer(adopter[operator]),
    post = post,                                  # instrument deployed midway
    hours_to_resolution = round(hours, 1),
    on_time = as.integer(hours <= deadline),
    stringsAsFactors = FALSE)
}

#' Draw covariates from the Synthea member cohort instead of inventing them.
#'
#' The earlier version drew complexity from rnorm(5, 2) — a distribution
#' chosen so the demo would work. Here it comes from a member's actual
#' condition and encounter burden, expedited follows an urgent diagnosis, and
#' social barriers come from Synthea's SDOH findings. The covariates are then
#' correlated the way Synthea's disease models correlate them rather than the
#' way I assumed, which is the point: matching is easy when confounders are
#' independent and this is a harder, fairer test of it.
#'
#' Members are sampled WITH replacement because 165 members must supply
#' several hundred grievances, and a member can file more than one.
#' `member_id` is kept so the interference check can see repeats.
cohort_path <- function()
  file.path(kb_root(), "analytics", "data", "member_cohort.csv")

simulate_from_cohort <- function(n = 900, true_effect = -9, seed = 42,
                                 deploy_after = 0.5, n_operators = 30,
                                 path = cohort_path()) {
  if (!file.exists(path))
    stop("no member cohort at ", path,
         " — run: python3 analytics/scripts/build_member_cohort.py", call. = FALSE)
  set.seed(seed)
  co <- utils::read.csv(path, stringsAsFactors = FALSE)
  idx <- sample(seq_len(nrow(co)), n, replace = TRUE)
  m <- co[idx, , drop = FALSE]

  # Condition and utilisation burden -> case complexity on a 1-10 scale.
  raw <- scale(log1p(m$conditions) + 0.6 * log1p(m$encounters))[, 1]
  complexity <- round(pmin(pmax(5 + 1.8 * raw, 1), 10), 1)
  expedited  <- m$urgent_condition
  sdoh       <- pmin(m$sdoh_flags, 6)

  backlog <- round(pmax(rnorm(n, 40, 14), 5))      # operational, not clinical
  tenure  <- round(pmax(rnorm(n, 3.2, 1.8), 0.2), 1)
  day     <- sample(seq_len(n))
  operator <- sprintf("op-%02d", sample(seq_len(n_operators), n, replace = TRUE))
  op_effect <- setNames(rnorm(n_operators, 0, 12), sprintf("op-%02d", seq_len(n_operators)))
  adopter <- setNames(rbinom(n_operators, 1, 0.5), sprintf("op-%02d", seq_len(n_operators)))

  post <- as.integer(day > deploy_after * n)
  # Assignment must leave OVERLAP. An earlier tuning put 84% of complex
  # cases into assistance, which left 299 treated units above propensity 0.8
  # against 34 controls — no comparable unassisted case exists for most of
  # the treated group, and no adjustment method can repair that. Assistance
  # is a minority behaviour in practice; these coefficients keep it around a
  # third of cases with support across the range.
  p_assist <- plogis(-3.2 + 0.20 * complexity + 0.45 * expedited +
                     0.010 * backlog - 0.14 * tenure + 0.16 * sdoh +
                     0.9 * post * adopter[operator])
  assisted <- rbinom(n, 1, p_assist)

  base <- 130 + 25 * complexity + 0.85 * backlog - 9 * tenure - 55 * expedited +
          14 * sdoh + op_effect[operator]
  hours <- pmax(base + true_effect * assisted + rnorm(n, 0, 34), 2)
  deadline <- ifelse(expedited == 1, 72, 720)

  data.frame(
    case_id = sprintf("SIM-%04d", seq_len(n)),
    member_id = m$member_id,
    simulated = TRUE,
    assisted = assisted,
    complexity = complexity,
    expedited = expedited,
    sdoh = sdoh,
    backlog = backlog,
    tenure = tenure,
    arrival = day,
    operator = operator,
    adopter = as.integer(adopter[operator]),
    post = post,
    hours_to_resolution = round(hours, 1),
    on_time = as.integer(hours <= deadline),
    stringsAsFactors = FALSE)
}

#' What the analysis frame looks like, wherever it came from.
describe_frame <- function(df) {
  if (is.null(df) || !nrow(df)) return(NULL)
  list(
    cases = nrow(df),
    simulated = all(df$simulated),
    assisted = sum(df$assisted == 1),
    unassisted = sum(df$assisted == 0),
    on_time_rate = if ("on_time" %in% names(df)) mean(df$on_time) else NA_real_,
    naive_diff = if ("hours_to_resolution" %in% names(df))
      mean(df$hours_to_resolution[df$assisted == 1], na.rm = TRUE) -
      mean(df$hours_to_resolution[df$assisted == 0], na.rm = TRUE) else NA_real_)
}
