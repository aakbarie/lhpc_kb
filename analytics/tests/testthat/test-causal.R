# Estimation. The simulation has a known true effect, so these assert
# recovery rather than merely that the code runs.

test_that("the member cohort loads and carries real Synthea structure", {
  skip_if_not(file.exists(cohort_path()), "cohort not built")
  co <- utils::read.csv(cohort_path())
  expect_gt(nrow(co), 100)
  expect_true(all(c("conditions", "encounters", "sdoh_flags",
                    "urgent_condition") %in% names(co)))
  # Real disease models produce skew; invented uniforms would not.
  expect_gt(max(co$encounters), 3 * stats::median(co$encounters))
})

test_that("the naive comparison is badly wrong and the adjusted one is not", {
  skip_if_not(file.exists(cohort_path()), "cohort not built")
  df <- simulate_from_cohort(n = 900, true_effect = -9, seed = 42)
  r <- run_analysis(df, true_effect = -9)
  # The confound is the point: assistance is sought on hard cases, so raw
  # means say it makes things worse.
  expect_gt(r$psm$naive, 10)
  # Adjusted estimates recover the truth.
  expect_true(r$recovered)
  expect_true(-9 >= r$iptw$ci[1] && -9 <= r$iptw$ci[2])
  # Sign is right, unlike the naive figure.
  expect_lt(r$psm$estimate, 0)
})

test_that("matching produces comparable groups", {
  skip_if_not(file.exists(cohort_path()), "cohort not built")
  r <- run_analysis(simulate_from_cohort(n = 900, seed = 42), true_effect = -9)
  expect_lt(r$psm$worst_balance, 0.1)      # the conventional bar
  expect_true(is.finite(r$psm$se) && r$psm$se > 0)
  # A zero-width interval is the most confident possible way to be wrong.
  expect_gt(r$psm$ci[2] - r$psm$ci[1], 0)
})

test_that("overlap failure is detected rather than estimated through", {
  skip_if_not(file.exists(cohort_path()), "cohort not built")
  df <- simulate_from_cohort(n = 600, seed = 7)
  expect_true(check_overlap(df)$ok)
  # Deterministic assignment on a covariate destroys common support.
  bad <- df; bad$assisted <- as.integer(bad$complexity > stats::median(bad$complexity))
  o <- suppressWarnings(check_overlap(bad))
  expect_false(o$ok)
  expect_gt(o$treated_outside, 0)
})

test_that("DiD needs a panel and says so when it has none", {
  skip_if_not(file.exists(cohort_path()), "cohort not built")
  df <- simulate_from_cohort(n = 500, seed = 3)
  expect_false(is.null(estimate_did(df)))
  expect_null(estimate_did(df[, setdiff(names(df), "adopter")]))
})
