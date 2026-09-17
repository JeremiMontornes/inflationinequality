#!/usr/bin/env Rscript

# Run a source-package check without propagating Unix-only C.UTF-8 locale
# settings to child R processes on Windows.

locale_vars <- c("LC_ALL", "LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_TIME")
if (.Platform$OS.type == "windows") {
  inherited <- Sys.getenv(locale_vars, unset = NA_character_)
  invalid <- !is.na(inherited) & grepl("^C\\.UTF-?8$", inherited, ignore.case = TRUE)
  if (any(invalid)) Sys.unsetenv(locale_vars[invalid])
}

repo <- normalizePath(file.path(dirname(sub("^--file=", "", grep(
  "^--file=", commandArgs(FALSE), value = TRUE
)[1L])), ".."), mustWork = TRUE)
audit_root <- file.path(tempdir(), "inflationinequality-check")
dir.create(audit_root, recursive = TRUE, showWarnings = FALSE)

old <- setwd(audit_root)
on.exit(setwd(old), add = TRUE)

r <- file.path(R.home("bin"), "R")
build_status <- system2(r, c("CMD", "build", shQuote(repo), "--no-build-vignettes"))
if (!identical(build_status, 0L)) quit(status = build_status)

description <- read.dcf(file.path(repo, "DESCRIPTION"))
tarball <- sprintf("inflationinequality_%s.tar.gz", description[1L, "Version"])
check_status <- system2(r, c("CMD", "check", shQuote(tarball), "--no-manual"))
quit(status = check_status)
