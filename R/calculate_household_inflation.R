#' Calculate household-level inflation from Spanish EPF microdata
#'
#' @description
#' `calculate_household_inflation()` builds household-specific HICP Laspeyres
#' indices by combining INE EPF household expenditure microdata with Eurostat
#' HICP item indices and official HICP item weights.
#'
#' @details
#' The function currently supports Spain (`country = "ES"`). It expects the INE
#' EPF ZIP files `datos_YYYY.zip` in `zip_dir`. If `download = TRUE`, missing
#' ZIPs are downloaded from the official INE microdata endpoint.
#'
#' For each household and EPF wave, the function reads `EPFhogar_YYYY.csv` and
#' `EPFgastos_YYYY.csv`, keeps household weights and expenditure lines, builds
#' a household-level HBS object, and passes it to [calculate_price_indices()].
#' Household baskets are matched to the official HICP annual item weights using
#' the same machinery as [calculate_weights()]. The output therefore follows the
#' package convention: EPF waves are used as HBS waves and matched to HICP
#' weight years by latest available prior wave.
#'
#' @param country Country code. Only `"ES"` is currently implemented.
#' @param years EPF microdata years to use. The default is `2020:2024`.
#' @param level COICOP level. Defaults to `3`, i.e. 4-digit class level.
#' @param start_year,start_month,end_year,end_month date range for inflation.
#' @param zip_dir directory where INE EPF `datos_YYYY.zip` files are stored.
#' @param zip_paths optional named or unnamed vector of explicit EPF ZIP paths.
#'   When supplied, it is used instead of `zip_dir`/`download`. File names can
#'   follow either INE's `datos_YYYY.zip` convention or a local convention such
#'   as `ine_epf_YYYY.zip`.
#' @param download if `TRUE`, download missing ZIP files from INE.
#' @param overwrite if `TRUE`, re-download existing ZIP files.
#' @param base_year year used to rebase chained price indices.
#' @param include_price_indices if `TRUE`, include the full price-index object
#'   in the returned list.
#' @param recode_ecoicop2_to_ecoicop1 passed to [calculate_price_indices()].
#' @param ensure_complete_cpi passed to [calculate_price_indices()].
#'
#' @returns An object of class `"household_inflation"` containing:
#' - `dt`: household-month inflation rates with columns `household_id`,
#'   `year`, `month`, `date`, `inflation`, `price_index`, and HBS metadata.
#' - `households`: EPF household metadata.
#' - `hbs`: household-level HBS object used in the calculation.
#' - `price_indices`: included only when `include_price_indices = TRUE`.
#'
#' @examples
#' \dontrun{
#' es_hh <- calculate_household_inflation(
#'   years = 2020:2024,
#'   start_year = 2021,
#'   end_year = 2024,
#'   download = TRUE
#' )
#' es_hh$dt
#' }
#'
#' @seealso [calculate_price_indices()], [calculate_weights()]
#' @export
calculate_household_inflation <- function(country = "ES",
                                          years = 2020:2024,
                                          level = 3,
                                          start_year = NULL,
                                          start_month = NULL,
                                          end_year = NULL,
                                          end_month = NULL,
                                          zip_dir = spain_epf_zip_dir(),
                                          zip_paths = NULL,
                                          download = FALSE,
                                          overwrite = FALSE,
                                          base_year = NULL,
                                          include_price_indices = FALSE,
                                          recode_ecoicop2_to_ecoicop1 = TRUE,
                                          ensure_complete_cpi = FALSE) {
  country <- toupper(country)
  if (!identical(country, "ES")) {
    stop("Only country = 'ES' is currently implemented for household-level EPF inflation.")
  }
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L || anyNA(years)) {
    stop("'years' must contain at least one valid EPF year.")
  }
  if (!identical(as.numeric(level), 3)) {
    stop("Spain EPF household-level inflation currently supports level = 3 only.")
  }

  zip_paths <- if (is.null(zip_paths)) {
    ensure_spain_epf_zips(
      years = years,
      zip_dir = zip_dir,
      download = download,
      overwrite = overwrite
    )
  } else {
    normalize_spain_epf_zip_paths(zip_paths = zip_paths, years = years)
  }
  epf <- load_spain_epf_household_microdata(zip_paths = zip_paths, level = level)

  price_indices <- calculate_price_indices(
    country = country,
    category = "household",
    level = level,
    start_year = start_year,
    start_month = start_month,
    end_year = end_year,
    end_month = end_month,
    ensure_complete_cpi = ensure_complete_cpi,
    custom_hbs = epf$hbs,
    base_year = base_year,
    include_total = FALSE,
    recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
  )

  out <- data.table::copy(price_indices$dt)
  data.table::setnames(out, "category", "household_id")
  data.table::setnames(out, "annual_rate", "inflation")
  out <- epf$households[
    out,
    on = "household_id"
  ]
  data.table::setcolorder(
    out,
    intersect(
      c(
        "household_id", "epf_year", "year", "month", "date", "inflation",
        "price_index", "laspeyres", "chain_laspeyres", "weight",
        "equivalised_income", "age_ref", "urban_group"
      ),
      names(out)
    )
  )

  result <- list(
    dt = out,
    households = epf$households,
    hbs = epf$hbs,
    country = country,
    level = level,
    epf_years = years,
    start_year = if (nrow(out) > 0L) min(out$year, na.rm = TRUE) else NA_integer_,
    end_year = if (nrow(out) > 0L) max(out$year, na.rm = TRUE) else NA_integer_
  )
  if (isTRUE(include_price_indices)) {
    result$price_indices <- price_indices
  }
  structure(result, class = "household_inflation")
}

spain_epf_zip_dir <- function() {
  path <- system.file("extdata", "spain_epf_zips", package = "inflationinequality", mustWork = FALSE)
  if (nzchar(path)) {
    return(path)
  }
  package_inst <- system.file(package = "inflationinequality")
  candidates <- c(
    if (nzchar(package_inst)) file.path(dirname(package_inst), "data-raw", "spain_epf", "zips"),
    file.path("data-raw", "spain_epf", "zips")
  )
  hit <- candidates[dir.exists(candidates)]
  if (length(hit) > 0L) {
    hit[[1L]]
  } else {
    candidates[[1L]]
  }
}

spain_epf_zip_url <- function(year) {
  sprintf("https://www.ine.es/ftp/microdatos/epf2006/datos_%s.zip", as.integer(year))
}

ensure_spain_epf_zips <- function(years, zip_dir, download = FALSE, overwrite = FALSE) {
  dir.create(zip_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- vapply(years, resolve_spain_epf_zip_path, character(1), zip_dir = zip_dir)
  missing <- !file.exists(paths) | isTRUE(overwrite)

  if (any(missing) && !isTRUE(download)) {
    stop(
      "Missing INE EPF ZIP file(s): ",
      paste(normalizePath(paths[missing], winslash = "/", mustWork = FALSE), collapse = ", "),
      ". Set download = TRUE to download them from INE.",
      call. = FALSE
    )
  }

  if (any(missing)) {
    for (i in which(missing)) {
      utils::download.file(
        url = spain_epf_zip_url(years[[i]]),
        destfile = paths[[i]],
        mode = "wb",
        quiet = FALSE
      )
    }
  }

  paths
}

resolve_spain_epf_zip_path <- function(year, zip_dir) {
  candidates <- file.path(
    zip_dir,
    c(sprintf("datos_%s.zip", year), sprintf("ine_epf_%s.zip", year))
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0L) {
    hit[[1L]]
  } else {
    candidates[[1L]]
  }
}

normalize_spain_epf_zip_paths <- function(zip_paths, years) {
  if (is.null(names(zip_paths)) || !all(as.character(years) %in% names(zip_paths))) {
    inferred_years <- as.integer(sub(".*(datos|ine_epf)_([0-9]{4})\\.zip$", "\\2", basename(zip_paths)))
    names(zip_paths) <- inferred_years
  }
  paths <- unname(zip_paths[as.character(years)])
  if (anyNA(paths)) {
    stop("Could not match 'zip_paths' to requested EPF years: ", paste(years, collapse = ", "))
  }
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop("Missing EPF ZIP path(s): ", paste(paths[missing], collapse = ", "))
  }
  paths
}

load_spain_epf_household_microdata <- function(zip_paths, level = 3) {
  pieces <- lapply(zip_paths, read_spain_epf_household_year, level = level)
  households <- data.table::rbindlist(lapply(pieces, `[[`, "households"), use.names = TRUE, fill = TRUE)
  expenses <- data.table::rbindlist(lapply(pieces, `[[`, "expenses"), use.names = TRUE, fill = TRUE)
  hbs_obj <- make_spain_epf_household_hbs(households = households, expenses = expenses, level = level)
  list(households = households, expenses = expenses, hbs = add_spain_epf_parent_coicop_rows(hbs_obj))
}

read_spain_epf_household_year <- function(zip_path, level = 3) {
  zip_path <- normalizePath(zip_path, winslash = "/", mustWork = TRUE)
  year <- as.integer(sub(".*(datos|ine_epf)_([0-9]{4})\\.zip$", "\\2", basename(zip_path)))
  if (is.na(year)) {
    stop("Cannot infer EPF year from ZIP file name: ", zip_path)
  }

  work_dir <- tempfile(sprintf("ine_epf_%s_", year))
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  outer_files <- utils::unzip(zip_path, list = TRUE)$Name
  target_nested <- outer_files[
    grepl(sprintf("^EPF(hogar|gastos)_%s\\.zip$", year), basename(outer_files))
  ]
  if (length(target_nested) < 2L) {
    stop("Could not find EPFhogar/EPFgastos nested ZIP files in ", zip_path)
  }
  utils::unzip(zip_path, files = target_nested, exdir = work_dir)
  nested_zips <- list.files(work_dir, pattern = "\\.zip$", recursive = TRUE, full.names = TRUE)
  for (nested_zip in nested_zips) {
    utils::unzip(nested_zip, exdir = file.path(work_dir, tools::file_path_sans_ext(basename(nested_zip))))
  }

  hogar_file <- find_spain_epf_csv(work_dir, sprintf("EPFhogar_%s.csv", year))
  gastos_file <- find_spain_epf_csv(work_dir, sprintf("EPFgastos_%s.csv", year))

  households <- data.table::fread(
    hogar_file,
    sep = "\t",
    na.strings = c("", "NA"),
    showProgress = FALSE
  )
  expenses <- data.table::fread(
    gastos_file,
    sep = "\t",
    colClasses = c(CODIGO = "character"),
    na.strings = c("", "NA"),
    showProgress = FALSE
  )

  require_spain_epf_columns(households, c("ANOENC", "NUMERO", "FACTOR"))
  require_spain_epf_columns(expenses, c("ANOENC", "NUMERO", "CODIGO", "GASTO"))

  keep_household_cols <- intersect(
    c("ANOENC", "NUMERO", "FACTOR", "UC1", "IMPEXAC", "EDADSP", "DENSIDAD"),
    names(households)
  )
  households <- households[, ..keep_household_cols]
  households <- households[!is.na(NUMERO) & !is.na(FACTOR) & FACTOR > 0]
  households[, `:=`(
    epf_year = year,
    household_id = paste("ES", ANOENC, NUMERO, sep = "_"),
    weight = FACTOR
  )]
  if (all(c("UC1", "IMPEXAC") %in% names(households))) {
    households[!is.na(UC1) & UC1 > 0 & !is.na(IMPEXAC) & IMPEXAC > 0,
      equivalised_income := IMPEXAC / UC1
    ]
  }
  if ("EDADSP" %in% names(households)) {
    households[, age_ref := data.table::fifelse(EDADSP < 30, "Less than 30 years",
      data.table::fifelse(EDADSP < 45, "From 30 to 44 years",
        data.table::fifelse(EDADSP < 60, "From 45 to 59 years",
          data.table::fifelse(EDADSP >= 60, "60 years or over", NA_character_)
        )
      )
    )]
  }
  if ("DENSIDAD" %in% names(households)) {
    households[, urban_group := data.table::fifelse(DENSIDAD == 3L, "Rural areas",
      data.table::fifelse(DENSIDAD == 2L, "Towns and suburbs",
        data.table::fifelse(DENSIDAD == 1L, "Cities", NA_character_)
      )
    )]
  }
  households <- households[, intersect(
    c("household_id", "epf_year", "weight", "equivalised_income", "age_ref", "urban_group"),
    names(households)
  ), with = FALSE]

  expenses <- expenses[!is.na(CODIGO) & !is.na(GASTO) & GASTO >= 0]
  expenses[, `:=`(
    epf_year = year,
    household_id = paste("ES", ANOENC, NUMERO, sep = "_"),
    coicop = substr(normalize_spain_epf_code(CODIGO, level = level), 1L, level + 1L),
    expenditure = GASTO / 1000
  )]
  expenses <- expenses[nchar(coicop) == level + 1L]
  expenses <- expenses[, .(expenditure = sum(expenditure, na.rm = TRUE)),
                       by = .(household_id, epf_year, coicop)]

  list(households = households, expenses = expenses)
}

find_spain_epf_csv <- function(work_dir, file_name) {
  files <- list.files(work_dir, pattern = paste0("^", file_name, "$"), recursive = TRUE, full.names = TRUE)
  if (length(files) == 0L) {
    stop("Could not find ", file_name, " inside extracted INE EPF ZIP.")
  }
  files[[1L]]
}

require_spain_epf_columns <- function(dt, cols) {
  missing <- setdiff(cols, names(dt))
  if (length(missing) > 0L) {
    stop("INE EPF file is missing required column(s): ", paste(missing, collapse = ", "))
  }
}

normalize_spain_epf_code <- function(code, level = 3) {
  code <- trimws(as.character(code))
  min_width <- level + 2L
  short <- !is.na(code) & nchar(code) < min_width
  code[short] <- paste0(
    strrep("0", min_width - nchar(code[short])),
    code[short]
  )
  code
}

make_spain_epf_household_hbs <- function(households, expenses, level = 3) {
  households <- data.table::copy(households)
  expenses <- data.table::copy(expenses)
  expenses <- expenses[households[, .(household_id, epf_year, weight)], on = .(household_id, epf_year), nomatch = 0]

  dt <- expenses[
    ,
    .(consumption = sum(expenditure, na.rm = TRUE)),
    by = .(coicop, epf_year, household_id)
  ]
  data.table::setnames(dt, c("epf_year", "household_id"), c("year", "category"))
  dt[, series_name := paste("ES EPF household", year, coicop, category, sep = ".")]
  dt <- dt[, .(series_name, coicop, year, category, consumption)]

  dt_total <- expenses[
    ,
    .(total_consumption = sum(expenditure * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)),
    by = .(coicop, epf_year)
  ]
  data.table::setnames(dt_total, "epf_year", "year")
  dt_total[, series_name := paste("ES EPF household total", year, coicop, sep = ".")]
  dt_total <- dt_total[, .(series_name, coicop, year, total_consumption)]

  hbs(
    dt = dt,
    dt_total = dt_total,
    country = "ES",
    category = "household",
    categories = sort(unique(dt$category)),
    level = level
  )
}
