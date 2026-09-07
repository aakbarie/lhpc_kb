# The Measurement Contract is a pre-registration, so it is tested as one:
# the diagram, fields, estimators and ROI formula must all be present and
# must drive the analysis rather than describe it.

test_that("the contract loads with every committed section", {
  ct <- measurement_contract()
  for (sec in c("contract", "intervention", "outcome", "dag",
                "required_fields", "rollout", "estimators", "roi"))
    expect_false(is.null(ct[[sec]]), info = sec)
  expect_equal(ct$dag$exposure, "assisted")
  expect_equal(ct$outcome$primary$name, "hours_to_resolution")
  expect_setequal(vapply(ct$estimators, `[[`, "", "name"), c("psm", "iptw", "did"))
})

test_that("a contract missing its diagram is refused, not half-loaded", {
  d <- file.path(tempdir(), "ct"); dir.create(d, showWarnings = FALSE)
  writeLines(c("contract: {id: x}", "intervention: {name: x}", "outcome: {}",
               "dag: {exposure: a, outcome: b}", "required_fields: []",
               "rollout: {}", "estimators: []", "roi: {}"),
             file.path(d, "x.measurement.yml"))
  expect_error(measurement_contract("x", dir = d), "no causal diagram")
  writeLines(c("contract: {id: y}"), file.path(d, "y.measurement.yml"))
  expect_error(measurement_contract("y", dir = d), "missing the `intervention`")
})

test_that("the adjustment set is derived from the contract's diagram", {
  ct <- measurement_contract()
  expect_equal(nrow(contract_dag(ct)), 12)
  # Editing the contract changes the analysis — that is the point of it
  # being a pre-registration rather than a comment.
  s <- adjustment_set(contract_dag(ct))
  expect_setequal(s$adjust, c("complexity", "expedited", "backlog", "tenure", "sdoh"))
})

test_that("readiness distinguishes a defect from a declared limit", {
  ct <- measurement_contract()
  # Nothing recorded: every field is a defect.
  bare <- data.frame(case_id = "a", operator = "op", stringsAsFactors = FALSE)
  r1 <- contract_readiness(bare, ct)
  expect_false(r1$ready)
  expect_true("complexity" %in% r1$defects)

  # Everything the deployment CAN record, recorded: sdoh remains, but as a
  # known limit rather than a defect — the contract already says there is no
  # connector for it.
  ok <- data.frame(case_id = paste0("c", 1:40), operator = rep(paste0("op", 1:10), 4),
                   complexity = 5, expedited = 0, backlog = 3, tenure = 2.1,
                   sdoh = NA_real_, stringsAsFactors = FALSE)
  r2 <- contract_readiness(ok, ct)
  expect_length(r2$defects, 0)
  expect_equal(r2$known_limits, "sdoh")
  expect_false(r2$ready)          # still refuses — an unmeasured confounder
  expect_match(r2$message, "connector")
})

test_that("readiness enforces the contracted rollout minimums", {
  ct <- measurement_contract()
  full <- function(n, ops) data.frame(
    case_id = paste0("c", seq_len(n)),
    operator = rep(paste0("op", seq_len(ops)), length.out = n),
    complexity = 5, expedited = 0, backlog = 3, tenure = 2.1, sdoh = 1,
    stringsAsFactors = FALSE)
  expect_true(contract_readiness(full(40, 10), ct)$ready)
  expect_false(contract_readiness(full(10, 10), ct)$ready)   # too few cases
  expect_match(contract_readiness(full(40, 3), ct)$message, "operators")
})

test_that("ROI is an interval and admits when it crosses zero", {
  ct <- measurement_contract()
  # An effect interval spanning zero must produce a return spanning the cost.
  spanning <- contract_roi(c(-15.4, 2.0), ct)
  expect_true(spanning$crosses_zero)
  expect_lt(spanning$net_annual_benefit[1], 0)
  expect_gt(spanning$net_annual_benefit[2], 0)
  expect_length(spanning$net_annual_benefit, 2)

  # A clearly beneficial effect does not.
  good <- contract_roi(c(-20, -10), ct)
  expect_false(good$crosses_zero)
  expect_gt(good$net_annual_benefit[1], 0)

  # Break-even is a property of the inputs, not of the estimate.
  expect_equal(spanning$break_even_hours_per_case, good$break_even_hours_per_case)
  expect_gt(spanning$break_even_hours_per_case, 0)
})

test_that("the analysis refuses to estimate when the contract is unmet", {
  skip_if_not(file.exists(cohort_path()), "cohort not built")
  df <- simulate_from_cohort(n = 200, seed = 5)
  blocked <- run_analysis(df[, setdiff(names(df), "tenure")])
  expect_true(isTRUE(blocked$blocked))
  expect_null(blocked$psm)
  expect_match(blocked$readiness$message, "tenure")
})
