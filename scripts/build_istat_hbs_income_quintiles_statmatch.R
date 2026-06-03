# Build Italy HBS income quintiles by statistical matching with EU-SILC
#
# Methodological target:
#   HBS Istat = recipient file with detailed consumption.
#   EU-SILC Italy = donor file with household income.
#   StatMatch = donor imputation of income into HBS on common variables.
#
# This follows the Istat statistical-matching approach used in the
# EU-SILC/HBS integration work by Donatiello et al. and related Istat papers:
# harmonise common variables, match donor households to recipient households,
# impute income, then build income quintiles in the recipient HBS.
#
# The script is deliberately outside R/ and inst/extdata while the microdata
# workflow is being tested.

suppressPackageStartupMessages({
  library(data.table)
  library(StatMatch)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

cfg <- list(
  year = 2020L,
  hbs_zip = file.path(
    Sys.getenv("ISTAT_HBS_DIR", "C:/Users/jerem/Documents/New project/data/istat_hbs"),
    "HBS_2020_RICOSTRUITICOICOP2018_IT.zip"
  ),
  # Set this to the Italian EU-SILC cross-sectional microdata file.
  # Supported formats: csv, txt, dta, sav, rds.
  silc_file = Sys.getenv("ISTAT_SILC_FILE", ""),
  out_dir = file.path("data-raw", "istat_hbs_statmatch"),
  level = 2L,
  seed = 202505L,

  # Edit this mapping after inspecting the HBS/SILC metadata.
  # The script stops with a useful variable-name report if a mapped variable is
  # not found. It only reads the mapped variables from each source.
  hbs_vars = list(
    household_id = "ID_FAM",
    weight = "PESO",
    region = "REG",
    household_size = "NCOMP",
    age_ref = "ETA_PR",
    sex_ref = "SESSO_PR",
    education_ref = "TITSTU_PR",
    tenure = "TITOLO_GOD",
    urbanisation = "COM_TIPO"
  ),
  silc_vars = list(
    household_id = "DB030",
    weight = "DB090",
    income = "HY020",
    region = "DB040",
    household_size = "HX040",
    age_ref = "RX010",
    sex_ref = "RB090",
    education_ref = "PE040",
    tenure = "HH021",
    urbanisation = "DB100"
  ),

  # COICOP expenditure columns in the Istat HBS wide file are detected by
  # pattern. If needed, replace by an explicit character vector.
  coicop_expense_pattern = "^(COICOP|CP|C)[_\\.]?[0-9]{2,4}",
  common_match_vars = c(
    "region", "household_size_band", "age_ref_band",
    "sex_ref", "education_ref", "tenure", "urbanisation"
  )
)

zip_member <- function(zip_file, pattern) {
  members <- utils::unzip(zip_file, list = TRUE)
  hit <- members$Name[grepl(pattern, members$Name, ignore.case = TRUE)]
  if (length(hit) == 0L) {
    stop("No ZIP member matched pattern: ", pattern, call. = FALSE)
  }
  hit[[1L]]
}

fread_zip_member <- function(zip_file, member, select = NULL, ...) {
  tmp <- tempfile(fileext = paste0("_", basename(member)))
  on.exit(unlink(tmp), add = TRUE)
  utils::unzip(zip_file, files = member, exdir = dirname(tmp), junkpaths = TRUE)
  extracted <- file.path(dirname(tmp), basename(member))
  on.exit(unlink(extracted), add = TRUE)
  data.table::fread(extracted, select = select, ...)
}

read_any_microdata <- function(path, select = NULL) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "txt", "tsv")) {
    return(data.table::fread(path, select = select))
  }
  if (ext == "rds") {
    x <- readRDS(path)
    dt <- data.table::as.data.table(x)
    return(dt[, intersect(select %||% names(dt), names(dt)), with = FALSE])
  }
  if (ext %in% c("dta", "sav")) {
    if (!requireNamespace("haven", quietly = TRUE)) {
      stop("Package 'haven' is required to read .", ext, " files.", call. = FALSE)
    }
    x <- if (ext == "dta") haven::read_dta(path) else haven::read_sav(path)
    dt <- data.table::as.data.table(x)
    return(dt[, intersect(select %||% names(dt), names(dt)), with = FALSE])
  }
  stop("Unsupported microdata format: ", ext, call. = FALSE)
}

assert_vars <- function(dt, vars, source_name) {
  missing <- setdiff(vars, names(dt))
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    source_name, " is missing mapped variable(s): ",
    paste(missing, collapse = ", "),
    "\nAvailable variables include:\n",
    paste(utils::head(names(dt), 80L), collapse = ", "),
    call. = FALSE
  )
}

rename_mapped <- function(dt, vars) {
  old <- unlist(vars, use.names = FALSE)
  new <- names(vars)
  data.table::setnames(dt, old = old, new = new, skip_absent = TRUE)
  dt
}

band_household_size <- function(x) {
  fifelse(x <= 1, "1",
    fifelse(x == 2, "2",
      fifelse(x == 3, "3", "4plus")
    )
  )
}

band_age <- function(x) {
  cut(
    as.numeric(x),
    breaks = c(-Inf, 29, 44, 59, 74, Inf),
    labels = c("under30", "30_44", "45_59", "60_74", "75plus"),
    right = TRUE
  )
}

harmonise_common_vars <- function(dt) {
  dt[, household_size_band := band_household_size(as.numeric(household_size))]
  dt[, age_ref_band := as.character(band_age(age_ref))]
  for (v in c("region", "sex_ref", "education_ref", "tenure", "urbanisation")) {
    dt[, (v) := as.character(get(v))]
  }
  dt
}

weighted_quintile <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) {
    return(rep(NA_integer_, length(x)))
  }
  ord <- order(x[ok])
  xo <- x[ok][ord]
  wo <- w[ok][ord]
  p <- cumsum(wo) / sum(wo)
  q <- findInterval(p, c(0.2, 0.4, 0.6, 0.8)) + 1L
  out <- rep(NA_integer_, length(x))
  out[which(ok)[ord]] <- q
  out
}

normalise_coicop <- function(x, level = 2L) {
  x <- gsub("[^0-9]", "", as.character(x))
  n <- level + 1L
  substr(x, 1L, pmin(nchar(x), n))
}

detect_hbs_vars <- function(zip_file, member, mapped_vars, coicop_pattern) {
  # Read only the header first. The Istat microdata text files are large.
  header <- fread_zip_member(zip_file, member, nrows = 0L)
  available <- names(header)
  mapped <- unlist(mapped_vars, use.names = FALSE)
  coicop <- available[grepl(coicop_pattern, available, ignore.case = TRUE)]
  unique(c(intersect(mapped, available), coicop))
}

build_statmatch_pairs <- function(hbs, silc, match_vars, seed) {
  set.seed(seed)
  hbs_match <- as.data.frame(hbs[, ..match_vars])
  silc_match <- as.data.frame(silc[, ..match_vars])

  # NND.hotdeck is deterministic conditional on the data and available matches.
  # The donation classes keep matches inside broad region and household-size
  # cells when possible; NND then uses a Gower distance on the remaining common
  # variables.
  StatMatch::NND.hotdeck(
    data.rec = hbs_match,
    data.don = silc_match,
    match.vars = match_vars,
    don.class = intersect(c("region", "household_size_band"), match_vars),
    dist.fun = "Gower",
    constrained = FALSE
  )
}

extract_donor_index <- function(match_object) {
  idx <- match_object$mtc.ids$ID.don
  if (is.null(idx)) {
    idx <- match_object$mtc.ids$don.id
  }
  if (is.null(idx)) {
    idx <- match_object$mtc.ids[, 2L]
  }
  as.integer(idx)
}

make_income_quintile_hbs <- function(hbs, year, level) {
  expense_cols <- names(hbs)[grepl(cfg$coicop_expense_pattern, names(hbs), ignore.case = TRUE)]
  if (length(expense_cols) == 0L) {
    stop("No COICOP expenditure columns detected in HBS.", call. = FALSE)
  }

  long <- melt(
    hbs,
    id.vars = c("household_id", "weight", "income_quintile"),
    measure.vars = expense_cols,
    variable.name = "coicop_raw",
    value.name = "expenditure",
    variable.factor = FALSE
  )
  long <- long[is.finite(expenditure) & expenditure > 0]
  long[, coicop := normalise_coicop(coicop_raw, level = level)]
  long[, expenditure_w := expenditure * as.numeric(weight)]

  dt <- long[!is.na(income_quintile), .(
    consumption = sum(expenditure_w, na.rm = TRUE)
  ), by = .(coicop, income_quintile)]
  dt[, `:=`(
    series_name = "ISTAT HBS with EU-SILC income imputed by StatMatch",
    year = as.numeric(year),
    category = paste0("Q", income_quintile)
  )]
  dt <- dt[, .(series_name, coicop, year, consumption, category)]

  dt_total <- long[, .(
    total_consumption = sum(expenditure_w, na.rm = TRUE)
  ), by = coicop]
  dt_total[, `:=`(
    series_name = "ISTAT HBS total",
    year = as.numeric(year)
  )]
  dt_total <- dt_total[, .(series_name, coicop, year, total_consumption)]

  categories <- paste0("Q", 1:5)
  inflationinequality::hbs(
    dt = dt,
    dt_total = dt_total,
    country = "IT",
    category = "income",
    categories = categories,
    level = level
  )
}

run <- function(cfg) {
  if (!nzchar(cfg$silc_file)) {
    stop(
      "Set cfg$silc_file or environment variable ISTAT_SILC_FILE to the ",
      "Italian EU-SILC cross-sectional microdata file.",
      call. = FALSE
    )
  }
  dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)

  member <- zip_member(cfg$hbs_zip, "MICRODATI/.*\\.txt$")
  hbs_select <- detect_hbs_vars(
    cfg$hbs_zip, member, cfg$hbs_vars, cfg$coicop_expense_pattern
  )
  silc_select <- unique(unlist(cfg$silc_vars, use.names = FALSE))

  hbs <- fread_zip_member(cfg$hbs_zip, member, select = hbs_select)
  silc <- read_any_microdata(cfg$silc_file, select = silc_select)

  assert_vars(hbs, unlist(cfg$hbs_vars, use.names = FALSE), "Istat HBS")
  assert_vars(silc, unlist(cfg$silc_vars, use.names = FALSE), "EU-SILC")

  hbs <- rename_mapped(hbs, cfg$hbs_vars)
  silc <- rename_mapped(silc, cfg$silc_vars)

  hbs <- harmonise_common_vars(hbs)
  silc <- harmonise_common_vars(silc)
  silc <- silc[is.finite(as.numeric(income)) & as.numeric(income) > 0]

  missing_match <- setdiff(cfg$common_match_vars, intersect(names(hbs), names(silc)))
  if (length(missing_match) > 0L) {
    stop("Common match variables missing after harmonisation: ",
         paste(missing_match, collapse = ", "), call. = FALSE)
  }

  match <- build_statmatch_pairs(hbs, silc, cfg$common_match_vars, cfg$seed)
  donor_idx <- extract_donor_index(match)
  hbs[, imputed_income := as.numeric(silc$income[donor_idx])]
  hbs[, donor_household_id := silc$household_id[donor_idx]]
  hbs[, income_quintile := weighted_quintile(imputed_income, as.numeric(weight))]

  hbs_obj <- make_income_quintile_hbs(hbs, cfg$year, cfg$level)

  out_hbs <- file.path(
    cfg$out_dir,
    sprintf("IT_HBS_%s_income_quintiles_statmatch_level%s.rds", cfg$year, cfg$level)
  )
  out_diag <- file.path(
    cfg$out_dir,
    sprintf("IT_HBS_%s_income_quintiles_statmatch_diagnostics.csv", cfg$year)
  )

  saveRDS(hbs_obj, out_hbs)
  diag <- hbs[, .(
    households = .N,
    weighted_households = sum(as.numeric(weight), na.rm = TRUE),
    mean_imputed_income = weighted.mean(imputed_income, as.numeric(weight), na.rm = TRUE)
  ), by = income_quintile][order(income_quintile)]
  fwrite(diag, out_diag)

  message("Wrote: ", normalizePath(out_hbs, winslash = "/", mustWork = FALSE))
  message("Wrote: ", normalizePath(out_diag, winslash = "/", mustWork = FALSE))
  invisible(list(hbs = hbs_obj, diagnostics = diag))
}

if (identical(environment(), globalenv())) {
  run(cfg)
}
