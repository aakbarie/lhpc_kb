# Control Passport tests. No network; needs data/nist_800_53.json.

skip_if_no_catalog <- function()
  testthat::skip_if_not(file.exists(file.path(kb_root(), "data", "nist_800_53.json")),
                        "NIST catalog not built")

test_that("the real NIST catalog parses to the published baseline sizes", {
  skip_if_no_catalog()
  cat <- nist_catalog()
  n <- function(b) sum(vapply(cat$controls,
                              function(c) b %in% (c$baselines %||% character()) , TRUE))
  # NIST Rev 5 published baseline sizes — if these drift, the parse broke.
  expect_equal(n("low"), 149)
  expect_equal(n("moderate"), 287)
  expect_equal(n("high"), 370)
  # Every non-withdrawn control must have statement prose; the nested-parts
  # parse silently yields empty statements if only the top level is read.
  active <- Filter(function(c) !isTRUE(c$withdrawn), cat$controls)
  expect_equal(sum(vapply(active, function(c) !nzchar(c$statement), TRUE)), 0)
})

test_that("a control already in the baseline cannot be claimed as tailoring", {
  skip_if_no_catalog()
  cat <- nist_catalog()
  spec <- list(passport = list(baseline = "low",
                               additions = list(list(id = "AC-3", reason = "x"))))
  # AC-3 is LOW baseline; claiming it as an addition overstates the hardening.
  expect_warning(ctl <- passport_controls(spec, cat), "already in the low baseline")
  expect_equal(sum(ctl$origin == "addition"), 0)
})

test_that("genuine tailoring is kept and labelled as an addition", {
  skip_if_no_catalog()
  cat <- nist_catalog()
  spec <- list(passport = list(baseline = "low",
                               additions = list(list(id = "AU-10", reason = "ledger"))))
  ctl <- passport_controls(spec, cat)          # AU-10 is HIGH-only in Rev 5
  expect_true("AU-10" %in% ctl$id)
  expect_equal(ctl$origin[ctl$id == "AU-10"], "addition")
  expect_match(ctl$reason[ctl$id == "AU-10"], "ledger")
})

test_that("the grievance passport reports its gaps rather than hiding them", {
  skip_if_no_catalog()
  p <- passport("grievance-timeliness")
  expect_s3_class(p, "kb_passport")
  expect_equal(p$counts$baseline, 149)
  expect_gt(p$counts$additions, 0)
  # The honest numbers: most parameters are unanswered and most controls are
  # asserted rather than evidenced. A passport claiming otherwise is wrong.
  expect_gt(p$counts$params_needed, p$counts$params_answered)
  expect_gt(p$counts$evidence_unbound, 0)
  expect_equal(length(p$open_parameters),
               p$counts$params_needed - p$counts$params_answered)
  expect_equal(p$counts$gates, 4)
  # Pre-operational: no artifact bound yet, but the spec is always hashed.
  expect_true(is.na(p$artifact_sha256))
  expect_match(p$spec_sha256, "^sha256:[0-9a-f]{64}$")
})
