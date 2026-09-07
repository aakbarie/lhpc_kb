# Instrument test runner. Sources the corpus helpers it depends on, then the
# instrument's own modules, then runs the instrument suite.
#
#   Rscript instrument/tests/run.R

library(testthat)

root <- local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  d
})

for (f in c("fetch_common.R", "config.R", "registry.R"))
  suppressPackageStartupMessages(source(file.path(root, "R", f)))
for (f in c("passport.R", "ledger.R", "independence.R", "cases.R", "assistant.R", "policies.R", "search.R"))
  suppressPackageStartupMessages(source(file.path(root, "instrument", "R", f)))

test_dir(file.path(root, "instrument", "tests", "testthat"), stop_on_failure = TRUE)
