# Test runner: sources every module, then runs the suite.
#
#   Rscript tests/run.R
#
# Exists because the tests cover pure functions spread across the fetch,
# ingest and graph modules, and sourcing only some of them produces
# "could not find function" errors that look like test failures but are not.

library(testthat)

root <- local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  d
})

for (f in c("fetch_common.R", "registry.R", "fetch_readily.R", "fetch_powerdms.R",
            "fetch_site.R", "fetch_manifest.R", "fetch.R", "config.R",
            "ingest.R", "graph.R", "smallworld.R", "passport.R"))
  suppressPackageStartupMessages(source(file.path(root, "R", f)))

test_dir(file.path(root, "tests", "testthat"), stop_on_failure = TRUE)
