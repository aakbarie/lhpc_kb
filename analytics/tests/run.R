# Analytics test runner.
#   Rscript analytics/tests/run.R
library(testthat)
root <- local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d) d <- dirname(d)
  d
})
for (f in c("fetch_common.R", "config.R"))
  suppressPackageStartupMessages(source(file.path(root, "R", f)))
for (f in c("passport.R", "ledger.R"))
  suppressPackageStartupMessages(source(file.path(root, "instrument", "R", f)))
for (f in c("dag.R", "dataset.R", "causal.R", "interpret.R"))
  suppressPackageStartupMessages(source(file.path(root, "analytics", "R", f)))
test_dir(file.path(root, "analytics", "tests", "testthat"), stop_on_failure = TRUE)
