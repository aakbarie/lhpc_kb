# The Measurement Contract.
#
# Sibling to the Control Passport. The Passport declares which controls govern
# an instrument; this declares what effect the instrument claims and how that
# claim will be tested — intervention, outcome, causal diagram, required
# fields, rollout design, estimators and ROI formula, all fixed before
# deployment.
#
# Why it is a contract and not an analysis plan: every field is a
# pre-registration. The diagram is fixed before the data exists, the
# estimators are named before anyone sees a result, and the ROI formula is
# agreed with the buyer rather than assembled afterwards from whatever moved.
# An evaluation designed after deployment can always be made to succeed. This
# one can fail, and the failure modes are enumerated in `preconditions`.
#
# It is also the specification the app must satisfy. `required_fields` is not
# documentation of what the instrument happens to record — it is what the
# instrument MUST record, and contract_readiness() refuses to estimate when a
# field is absent rather than quietly dropping a confounder.
#
# Shared by the instrument (which captures the fields) and the analytics app
# (which consumes them), so the two cannot disagree about the vocabulary.

suppressPackageStartupMessages({ library(yaml) })

#' Load a contract. Fails loudly on a malformed one — a measurement contract
#' that silently half-loads is worse than none, because the analysis proceeds.
measurement_contract <- function(id = "grievance-timeliness", dir = NULL) {
  # The measurement contract lives beside the instrument it measures, next to
  # the spec that drives the Control Passport — same instrument, two
  # pre-deployment commitments. An earlier layout put it in a second
  # top-level instruments/ directory, which shadowed the instrument's own and
  # silently broke every lookup that walks up looking for that name.
  if (is.null(dir)) {
    root <- local({
      d <- normalizePath(getwd(), winslash = "/")
      while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
        d <- dirname(d)
      d
    })
    dir <- file.path(root, "instrument", "instruments")
    if (!dir.exists(dir)) dir <- file.path(root, "instruments")
  }
  p <- file.path(dir, paste0(id, ".measurement.yml"))
  if (!file.exists(p)) stop("no measurement contract at ", p, call. = FALSE)
  y <- yaml::read_yaml(p)

  for (sec in c("contract", "intervention", "outcome", "dag",
                "required_fields", "rollout", "estimators", "roi"))
    if (is.null(y[[sec]]))
      stop("measurement contract is missing the `", sec, "` section: ", p,
           call. = FALSE)
  if (is.null(y$dag$edges) || !length(y$dag$edges))
    stop("contract declares no causal diagram; the adjustment set cannot be derived",
         call. = FALSE)
  y
}

#' The diagram as an edge frame, for the backdoor machinery.
contract_dag <- function(ct = measurement_contract()) {
  e <- ct$dag$edges
  data.frame(
    from = vapply(e, `[[`, "", "from"),
    to   = vapply(e, `[[`, "", "to"),
    note = vapply(e, function(x) x$note %||% "", ""),
    stringsAsFactors = FALSE)
}

#' Fields the instrument is contractually obliged to capture, and whether the
#' deployment can actually measure each one.
contract_fields <- function(ct = measurement_contract()) {
  f <- ct$required_fields
  data.frame(
    name = vapply(f, `[[`, "", "name"),
    at = vapply(f, function(x) x$at %||% "", ""),
    source = vapply(f, function(x) x$source %||% "", ""),
    measurable = vapply(f, function(x) isTRUE(x$measurable), TRUE),
    note = vapply(f, function(x) x$note %||% "", ""),
    stringsAsFactors = FALSE)
}

#' Can the contract be honoured against this data?
#'
#' Returns a verdict, not a warning. A missing confounder does not make an
#' estimate slightly worse — it makes it an estimate of a different quantity,
#' and the honest response is to decline. `blocking` separates fields the
#' contract already admits are unmeasurable in this deployment from fields the
#' app simply failed to record; the first is a known limit, the second is a
#' defect.
contract_readiness <- function(df, ct = measurement_contract()) {
  fields <- contract_fields(ct)
  need <- fields$name
  n <- if (is.null(df)) 0L else nrow(df)

  present <- intersect(need, names(df %||% data.frame()))
  absent <- setdiff(need, present)
  unmeasured <- present[vapply(present, function(c) all(is.na(df[[c]])), TRUE)]
  gaps <- union(absent, unmeasured)

  known_limit <- intersect(gaps, fields$name[!fields$measurable])
  defects <- setdiff(gaps, known_limit)

  min_n <- ct$rollout$minimum_cases %||% 30
  min_ops <- ct$rollout$minimum_operators %||% 0
  ops <- if (!is.null(df) && "operator" %in% names(df))
    length(unique(stats::na.omit(df$operator))) else 0L

  ready <- length(gaps) == 0 && n >= min_n && ops >= min_ops
  list(
    ready = ready,
    usable = setdiff(present, unmeasured),
    defects = defects, known_limits = known_limit,
    n = n, operators = ops, min_cases = min_n, min_operators = min_ops,
    message = if (length(defects))
      sprintf("Not measurable: the instrument is not recording %s. The contract requires it at intake.",
              paste(defects, collapse = ", "))
    else if (length(known_limit))
      sprintf("Not measurable: %s needs a connector this deployment does not have. The contract records it as unmeasurable; an estimate adjusting for the rest would not be the effect of the assistant.",
              paste(known_limit, collapse = ", "))
    else if (n < min_n)
      sprintf("Not yet: %d cases against a contracted minimum of %d.", n, min_n)
    else if (ops < min_ops)
      sprintf("Not yet: %d operators against a contracted minimum of %d — the rollout design needs a comparison group.",
              ops, min_ops)
    else sprintf("Contract satisfiable: %d cases, %d operators, all required fields recorded at intake.",
                 n, ops))
}

#' Check the estimate against the preconditions the contract declared.
#'
#' The contract names an overlap ratio and a balance threshold and says
#' `refuse_if_unmet`. Declaring a threshold and then publishing a number that
#' misses it is worse than declaring none — it lends the appearance of rigour
#' to a result the same document already called inadmissible. So this is
#' checked after estimation and before anything is reported.
contract_preconditions <- function(psm, overlap, ct = measurement_contract()) {
  pre <- ct$preconditions %||% list()
  bal_max <- pre$balance_max %||% Inf
  ov_max <- pre$overlap_ratio_max %||% Inf
  refuse <- isTRUE(pre$refuse_if_unmet)

  failures <- character()
  if (!is.null(psm) && is.finite(psm$worst_balance) && psm$worst_balance > bal_max)
    failures <- c(failures, sprintf(
      "covariate balance %.3f exceeds the contracted maximum of %.2f — the matched groups still differ on something the estimate would attribute to the assistant",
      psm$worst_balance, bal_max))
  if (!is.null(overlap) && is.finite(overlap$ratio_upper) && overlap$ratio_upper > ov_max)
    failures <- c(failures, sprintf(
      "overlap ratio %.1f exceeds the contracted maximum of %.0f — too few comparable untreated cases",
      overlap$ratio_upper, ov_max))

  list(met = length(failures) == 0, refuse = refuse, failures = failures,
       message = if (length(failures) == 0)
         "Preconditions met." else
         paste0("Preconditions not met: ", paste(failures, collapse = "; "),
                if (refuse) ". The contract requires the estimate be withheld." else ""))
}

#' ROI from the contract's formula and the estimated effect.
#'
#' The effect enters as an INTERVAL, never a point. The contract says so, and
#' the reason is that a point-estimate ROI reads as a promise: quoting a
#' single return from an effect whose interval crosses zero is the most
#' common way these numbers become untrue.
contract_roi <- function(hours_saved_ci, ct = measurement_contract()) {
  inp <- ct$roi$inputs
  v <- function(k) as.numeric(inp[[k]]$value)
  rate <- v("loaded_hourly_cost"); cases <- v("cases_per_year")
  breach <- v("cost_per_breach"); cost <- v("annual_cost")

  # Effect is reported as hours CHANGED (negative = faster), so saved hours
  # are the negation. Both interval ends are carried through.
  saved <- sort(-hours_saved_ci)
  benefit <- saved * cases * rate
  net <- benefit - cost
  list(
    inputs = list(loaded_hourly_cost = rate, cases_per_year = cases,
                  cost_per_breach = breach, annual_cost = cost),
    hours_saved = saved,
    net_annual_benefit = net,
    roi_ratio = net / cost,
    break_even_hours_per_case = cost / (cases * rate),
    crosses_zero = net[1] < 0 && net[2] > 0,
    formula = ct$roi$formula)
}
