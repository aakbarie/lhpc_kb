# Estimation: propensity score matching and difference-in-differences.
#
# The covariates are NOT chosen here. They come from adjustment_set() in
# dag.R, so the identification argument lives in the diagram and this file
# only executes it. Changing what is controlled for means changing the DAG,
# which means making a claim about the operation that someone can dispute.
#
# Every estimator reports the naive comparison beside the adjusted one,
# because on this problem they point in OPPOSITE directions — assistance is
# sought on hard cases, so raw means say it makes things worse. An analysis
# that showed only the adjusted number would be asking to be trusted; showing
# both shows the work.

suppressPackageStartupMessages({
  library(MatchIt); library(dplyr)
})

`%|NA|%` <- function(a, b) if (is.na(a)) b else a

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  source(file.path(d, "analytics", "R", "dag.R"))
  if (!exists("measurement_contract")) source(file.path(d, "R", "contract.R"))
})

#' Cluster-robust (CR1) standard errors.
#'
#' Hand-rolled because `sandwich` is not installed here. DiD residuals are
#' serially correlated within a unit, and classical SEs on panel data are
#' famously far too small — reporting one would make a null result look
#' significant.
cluster_se <- function(fit, cluster) {
  X <- stats::model.matrix(fit)
  u <- stats::residuals(fit)
  n <- nrow(X); k <- ncol(X); g <- length(unique(cluster))
  bread <- solve(crossprod(X))
  meat <- matrix(0, k, k)
  for (cl in unique(cluster)) {
    idx <- which(cluster == cl)
    Xu <- crossprod(X[idx, , drop = FALSE], u[idx])
    meat <- meat + tcrossprod(Xu)
  }
  adj <- (g / (g - 1)) * ((n - 1) / (n - k))
  sqrt(diag(bread %*% (adj * meat) %*% bread))
}

#' Common support between treated and control propensity scores.
#'
#' The check that decides whether an estimate is possible at all. If treated
#' units sit in a region containing almost no controls, matching will pair
#' them with whatever is nearest and report a number — the number is the
#' shape of the sample, not the effect. This is reported before any estimate
#' so a thin overlap is visible rather than inferred from bad balance.
check_overlap <- function(df, exposure = "assisted") {
  covs <- adjustment_set(outcome = "hours")$adjust
  f <- stats::as.formula(paste(exposure, "~", paste(covs, collapse = " + ")))
  ps <- stats::predict(stats::glm(f, data = df, family = stats::binomial()),
                       type = "response")
  t <- ps[df[[exposure]] == 1]; c <- ps[df[[exposure]] == 0]
  lo <- max(min(t), min(c)); hi <- min(max(t), max(c))
  # How lopsided is the dense end? Treated per control in the top region.
  hi_t <- sum(t > 0.8); hi_c <- sum(c > 0.8)
  # No treated units in the dense upper region means there is nothing to be
  # lopsided about. Reporting Inf there flagged a perfectly healthy overlap
  # as a violation — 0 treated against 0 controls is not a shortage.
  ratio <- if (hi_t == 0) 0 else if (hi_c > 0) hi_t / hi_c else Inf
  list(ps = ps, support = c(lo, hi),
       treated_outside = sum(t < lo | t > hi),
       ratio_upper = ratio,
       ok = ratio < 5,
       message = sprintf(
         "Common support [%.2f, %.2f]; %d treated outside it; %d treated vs %d controls above 0.8",
         lo, hi, sum(t < lo | t > hi), hi_t, hi_c))
}

#' Propensity score matching, with the adjustment set the DAG implies.
estimate_psm <- function(df, outcome = "hours_to_resolution",
                         exposure = "assisted", ratio = 1) {
  set <- adjustment_set(outcome = "hours")
  covs <- set$adjust
  check <- dag_check_model(covs)
  stopifnot(check$ok)

  f <- stats::as.formula(paste(exposure, "~", paste(covs, collapse = " + ")))
  # Two settings here were both forced by data rather than chosen.
  #
  # CALIPER. Without one, nearest-neighbour matching always finds a partner
  # however poor; the first run left a standardised mean difference of 0.355,
  # well above the 0.1 convention, leaving real confounding inside the
  # "adjusted" estimate. 0.2 SD of the propensity logit is the usual bound.
  #
  # REPLACEMENT. On the Synthea-grounded cohort the treated group is LARGER
  # than the control group — realistic covariates make assistance common —
  # and matching without replacement then silently discards treated units it
  # cannot pair, degrading balance to 0.205 and pushing the estimate to the
  # wrong side of zero. Reusing controls fixes the balance; the cost is that
  # a control can serve several treated units, so it is no longer an
  # independent observation and the weights must carry into both the fit and
  # the standard error.
  ov <- check_overlap(df, exposure)
  # Trim to common support before matching. Units with no counterpart cannot
  # inform a comparison, and keeping them only lets the matcher invent one.
  keep_rows <- ov$ps >= ov$support[1] & ov$ps <= ov$support[2]
  df <- df[keep_rows, , drop = FALSE]

  n_t <- sum(df[[exposure]] == 1); n_c <- sum(df[[exposure]] == 0)
  use_replace <- n_t > n_c * 0.8
  # Search matching specifications and keep the BEST-BALANCED one.
  #
  # Two mistakes were made here and both are worth recording. The first
  # version tried progressively tighter calipers and kept whichever it tried
  # LAST — so when none met the threshold it reported the worst, not the
  # best. The second added Mahalanobis matching on the covariates, which is
  # the textbook remedy for the propensity-score paradox (the score balanced
  # to 0.001 while backlog sat at -0.209, because two cases equally likely to
  # be assisted can differ on everything that produced that likelihood) — but
  # inside a 0.025 caliper it had almost nothing to choose from and made
  # balance worse.
  #
  # So: try both distances across a range of calipers, score each on the
  # balance the contract actually cares about, and keep the winner. If none
  # reaches the threshold that is a finding, not a reason to keep tuning.
  bal_max <- tryCatch(measurement_contract()$preconditions$balance_max,
                      error = function(e) NULL) %||% 0.1
  worst_of <- function(fit) suppressWarnings(
    max(abs(summary(fit)$sum.matched[, "Std. Mean Diff."]), na.rm = TRUE))
  mahf <- stats::as.formula(paste("~", paste(covs, collapse = " + ")))

  m <- NULL; caliper_used <- NA_real_; best <- Inf; used_mahalanobis <- FALSE
  for (mah in c(FALSE, TRUE)) {
    for (cal in c(0.5, 0.25, 0.2, 0.1, 0.05)) {
      cand <- tryCatch(MatchIt::matchit(
        f, data = df, method = "nearest", distance = "glm", ratio = ratio,
        replace = use_replace, caliper = cal,
        mahvars = if (mah) mahf else NULL), error = function(e) NULL)
      if (is.null(cand)) next
      wb <- worst_of(cand)
      if (!is.finite(wb) || wb >= best) next
      m <- cand; best <- wb; caliper_used <- cal; used_mahalanobis <- mah
    }
  }
  if (is.null(m)) stop("matching failed at every specification", call. = FALSE)
  md <- MatchIt::match.data(m)

  fit <- stats::lm(stats::as.formula(paste(outcome, "~", exposure)),
                   data = md, weights = md$weights)
  # Matched pairs are the cluster: two rows from one match are not
  # independent observations.
  # With replacement a control appears in several subclasses, so the pair
  # clustering degenerates and CR1 can return ~0 — which showed up as a
  # zero-width interval, the most confident possible way to be wrong. Fall
  # back whenever the robust SE is not a usable positive number.
  se_cl <- tryCatch(cluster_se(fit, md$subclass)[[exposure]],
                    error = function(e) NA_real_)
  se_cl <- if (is.finite(se_cl) && se_cl > 1e-8) se_cl else NA_real_
  se <- se_cl %|NA|% summary(fit)$coefficients[exposure, 2]
  est <- unname(stats::coef(fit)[[exposure]])

  naive <- mean(df[[outcome]][df[[exposure]] == 1], na.rm = TRUE) -
           mean(df[[outcome]][df[[exposure]] == 0], na.rm = TRUE)

  bal <- summary(m)$sum.matched
  list(
    overlap = ov,
    robust_se = !is.na(se_cl),
    caliper = caliper_used, mahalanobis = used_mahalanobis,
    method = sprintf("Propensity score matching (nearest neighbour%s, caliper %.3g%s)",
                     if (use_replace) ", with replacement" else "", caliper_used,
                     if (used_mahalanobis) ", Mahalanobis within caliper" else ""),
    replacement = use_replace,
    covariates = covs,
    n_matched = nrow(md), n_treated = sum(md[[exposure]] == 1),
    n_unmatched_treated = sum(df[[exposure]] == 1) - sum(md[[exposure]] == 1),
    estimate = est, se = se,
    ci = est + c(-1.96, 1.96) * se,
    p = 2 * stats::pnorm(-abs(est / se)),
    naive = naive,
    balance = data.frame(
      covariate = rownames(bal),
      std_diff_matched = round(bal[, "Std. Mean Diff."], 3),
      row.names = NULL),
    worst_balance = max(abs(bal[, "Std. Mean Diff."]), na.rm = TRUE),
    model = m)
}

#' Difference-in-differences across the instrument's deployment.
#'
#' The panel is OPERATORS, not cases. Adopters are those who took up the
#' assistant when it became available; the estimate is how their handling
#' time moved relative to non-adopters over the same period. Standard errors
#' cluster on operator, because one operator's cases are not independent
#' draws — with classical SEs a DiD on panel data reports intervals far too
#' narrow, which is how null results get published as findings.
estimate_did <- function(df, outcome = "hours_to_resolution") {
  if (!all(c("operator", "adopter", "post") %in% names(df)))
    return(NULL)                       # not a panel; DiD is not identified
  f <- stats::as.formula(paste(outcome, "~ adopter * post +",
                               paste(adjustment_set(outcome = "hours")$adjust,
                                     collapse = " + ")))
  fit <- stats::lm(f, data = df)
  term <- "adopter:post"
  se <- tryCatch(cluster_se(fit, df$operator)[[term]],
                 error = function(e) summary(fit)$coefficients[term, 2])
  est <- unname(stats::coef(fit)[[term]])
  # DiD here estimates something CLOSER TO INTENT-TO-TREAT than the matched
  # estimate does: it compares operators who ever adopted against those who
  # did not, and an adopter uses the assistant on only some of their cases.
  # So a smaller, often null, DiD alongside a clearly negative matched
  # estimate is the expected pattern, not a contradiction — the two answer
  # "what does offering the tool do" and "what does using it do".
  list(
    method = "Difference-in-differences, operator panel, cluster-robust SE (intent-to-treat)",
    estimand = "intent-to-treat: effect of adoption, diluted by non-use",
    clusters = length(unique(df$operator)),
    estimate = est, se = se,
    ci = est + c(-1.96, 1.96) * se,
    p = 2 * stats::pnorm(-abs(est / se)),
    fit = fit)
}

#' Inverse probability weighting, as an independent check on the matching.
#'
#' Matching discards units; weighting keeps all of them. They rest on the same
#' identification (the DAG) but fail differently — matching on poor overlap,
#' weighting on extreme weights — so agreement between them is worth more
#' than either alone, and disagreement is a signal to stop and look.
#'
#' ATT weights: treated units get 1, controls get p/(1-p). Weights are
#' trimmed at the 1st and 99th percentile, because a control with propensity
#' 0.99 otherwise carries the weight of a hundred cases.
estimate_iptw <- function(df, outcome = "hours_to_resolution",
                          exposure = "assisted") {
  covs <- adjustment_set(outcome = "hours")$adjust
  f <- stats::as.formula(paste(exposure, "~", paste(covs, collapse = " + ")))
  ps <- stats::predict(stats::glm(f, data = df, family = stats::binomial()),
                       type = "response")
  w <- ifelse(df[[exposure]] == 1, 1, ps / pmax(1 - ps, 1e-6))
  cap <- stats::quantile(w, c(0.01, 0.99), na.rm = TRUE)
  w <- pmin(pmax(w, cap[1]), cap[2])

  fit <- stats::lm(stats::as.formula(paste(outcome, "~", exposure)),
                   data = df, weights = w)
  se <- summary(fit)$coefficients[exposure, 2]
  est <- unname(stats::coef(fit)[[exposure]])
  list(method = "Inverse probability weighting (ATT, trimmed)",
       estimate = est, se = se, ci = est + c(-1.96, 1.96) * se,
       p = 2 * stats::pnorm(-abs(est / se)),
       max_weight = max(w))
}

#' Run both, and say plainly whether the adjusted estimate recovered truth.
#'
#' `true_effect` is available only on simulated data. Where it exists the
#' comparison is the point of the exercise: an estimator that cannot be
#' checked is a decoration.
run_analysis <- function(df, true_effect = NA_real_, ct = NULL) {
  ct <- ct %||% tryCatch(measurement_contract(), error = function(e) NULL)
  # The contract decides whether an estimate may be produced at all. Checked
  # BEFORE estimating, so a missing confounder is reported as a missing
  # confounder rather than discovered later as odd balance.
  ready <- if (!is.null(ct)) contract_readiness(df, ct) else NULL
  if (!is.null(ready) && !isTRUE(ready$ready))
    return(list(frame = describe_frame(df), readiness = ready, contract = ct,
                blocked = TRUE))
  psm <- estimate_psm(df)
  did <- tryCatch(estimate_did(df), error = function(e) NULL)
  iptw <- tryCatch(estimate_iptw(df), error = function(e) NULL)
  pre <- if (!is.null(ct)) contract_preconditions(psm, psm$overlap, ct) else NULL
  # A contract that names a threshold and then reports past it is worse than
  # one that names none, so an unmet precondition withholds the estimate.
  withheld <- !is.null(pre) && !pre$met && isTRUE(pre$refuse)
  list(
    preconditions = pre, withheld = withheld,
    frame = describe_frame(df),
    dag = adjustment_set(outcome = "hours"),
    psm = psm, did = did, iptw = iptw, overlap = psm$overlap,
    readiness = ready, contract = ct, blocked = FALSE,
    # ROI is derived from the interval, never the point estimate — the
    # contract requires it, because a single quoted return from an effect
    # whose interval crosses zero reads as a promise.
    roi = if (!is.null(ct) && !withheld)
            tryCatch(contract_roi(psm$ci, ct), error = function(e) NULL) else NULL,
    true_effect = true_effect,
    recovered = if (!is.na(true_effect))
      true_effect >= psm$ci[1] && true_effect <= psm$ci[2] else NA)
}
