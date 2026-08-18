suppressPackageStartupMessages({
  library(data.table)
})

zip_path <- file.path("data-raw", "ISTAT_MFR_EUSILC_Cross_2023_EN.zip")
if (!file.exists(zip_path)) {
  stop("Missing EU-SILC ZIP: ", zip_path, call. = FALSE)
}

zip_index <- unzip(zip_path, list = TRUE)
micro_files <- zip_index$Name[grepl("MICRODATI/.*\\.txt$", zip_index$Name)]
if (length(micro_files) == 0L) {
  stop("No MICRODATI txt files found in ", zip_path, call. = FALSE)
}

tmp <- tempfile("eusilc_2023_")
dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
unzip(zip_path, files = micro_files, exdir = tmp)

read_micro <- function(pattern) {
  file <- file.path(tmp, micro_files[grepl(pattern, micro_files)])
  if (length(file) != 1L) {
    stop("Expected one file for pattern ", pattern, "; found ", length(file), call. = FALSE)
  }
  fread(file, sep = "^", na.strings = c(".", ""), showProgress = FALSE)
}

d <- read_micro("HOUSEHOLD_REGISTER")
h <- read_micro("HOUSEHOLD_DATA")
r <- read_micro("PERSONAL_REGISTER")
p <- read_micro("PERSONAL_DATA")

file_summary <- data.table(
  file = basename(micro_files),
  rows = c(nrow(p), nrow(r), nrow(d), nrow(h)),
  cols = c(ncol(p), ncol(r), ncol(d), ncol(h)),
  file_structure_example = grepl("FILE_STRUCTURE_EXAMPLE", basename(micro_files))
)

hh <- merge(
  d,
  h,
  by.x = "DB030",
  by.y = "HB030",
  all = FALSE,
  allow.cartesian = TRUE
)

needed_household <- c(
  "DB030", # household id
  "DB040", # region / NUTS
  "DB090", # household weight
  "DB100", # degree of urbanisation
  "HY020", # total disposable household income
  "HY010", # total household gross income
  "HX040", # household size
  "HX090", # equivalised disposable income
  "HH021"  # tenure status
)
needed_person <- c(
  "RB030", # personal id
  "RB050", # personal weight
  "RB080", # year of birth
  "RB090", # sex
  "RX010", # age
  "RX040"  # household id
)

var_presence <- rbindlist(list(
  data.table(file = "D/H household merge", variable = needed_household,
             present = needed_household %in% names(hh)),
  data.table(file = "R personal register", variable = needed_person,
             present = needed_person %in% names(r))
))

diagnostic_vars <- intersect(
  c("DB090", "HY020", "HY010", "HX040", "HX090", "DB040", "DB100", "HH021"),
  names(hh)
)
var_diagnostics <- rbindlist(lapply(diagnostic_vars, function(v) {
  data.table(
    variable = v,
    non_missing = sum(!is.na(hh[[v]])),
    unique_values = uniqueN(hh[[v]], na.rm = TRUE),
    sample_values = paste(head(unique(hh[[v]]), 6L), collapse = " | ")
  )
}))

key_summary <- data.table(
  check = c(
    "D rows",
    "D unique DB030",
    "H rows",
    "H unique HB030",
    "D-H merged rows",
    "D-H merged unique households",
    "R rows",
    "R unique persons RB030",
    "P rows",
    "P unique persons PB030"
  ),
  value = c(
    nrow(d),
    uniqueN(d$DB030),
    nrow(h),
    uniqueN(h$HB030),
    nrow(hh),
    uniqueN(hh$DB030),
    nrow(r),
    uniqueN(r$RB030),
    nrow(p),
    uniqueN(p$PB030)
  )
)

cat("\n=== EU-SILC 2023 ZIP ===\n")
cat(normalizePath(zip_path, winslash = "/", mustWork = FALSE), "\n")

cat("\n=== File Summary ===\n")
print(file_summary)

cat("\n=== Key Summary ===\n")
print(key_summary)

cat("\n=== Variables Needed for Statistical Matching ===\n")
print(var_presence)

cat("\n=== Household Variable Diagnostics ===\n")
print(var_diagnostics)

cat("\n=== HY020 disposable income summary on D-H merge ===\n")
print(summary(as.numeric(hh$HY020)))

cat("\n=== DB090 household weight summary on D-H merge ===\n")
print(summary(as.numeric(hh$DB090)))

cat("\n=== Feasibility Assessment ===\n")
if (all(file_summary$file_structure_example) || max(file_summary$rows) < 1000L) {
  cat(
    "NOT sufficient for production statistical matching: the ZIP contains only ",
    "FILE_STRUCTURE_EXAMPLE microdata files with a very small number of rows. ",
    "It is useful to validate variable names and parsing, but not to estimate ",
    "income-consumption relationships.\n",
    sep = ""
  )
} else if (all(var_presence$present) && nrow(hh) > 1000L) {
  cat("Potentially feasible: required variables are present and row counts look like real microdata.\n")
} else {
  cat("Partially feasible: some required variables or row counts need review before matching.\n")
}
