#!/usr/bin/env Rscript

# Run tests normally skipped on CRAN. Several legacy tests perform large live
# Eurostat/HBS calculations without their own reachability guard, so require an
# explicit opt-in instead of accidentally turning an ordinary local test into a
# long network job.

if (!identical(tolower(Sys.getenv(
  "INFLATIONINEQUALITY_RUN_NETWORK_TESTS", "false"
)), "true")) {
  stop(
    "Integration tests use live external data. Set ",
    "INFLATIONINEQUALITY_RUN_NETWORK_TESTS=true to run them."
  )
}

Sys.setenv(NOT_CRAN = "true")
repo <- normalizePath(file.path(dirname(sub(
  "^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]
)), ".."), mustWork = TRUE)
old <- setwd(repo)
on.exit(setwd(old), add = TRUE)
pkgload::load_all(repo, quiet = TRUE)
testthat::test_dir("tests/testthat", reporter = "summary")
