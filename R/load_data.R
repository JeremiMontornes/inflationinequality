# BdF-specific
options(rdbnomics.use_readLines = TRUE)

#' Downloads monthly CPI (Consumer Price Index) data
#'
#' @description
#' `load_cpi()` downloads monthly CPI data from Eurostat's HICP (Harmonised Indices of Consumer Prices) database via DBnomics from a specified country. The user must specify the granularity of the product classification according to the COICOP nomenclature. A time period must also be specified.
#'
#' The data is formatted as `data.table` with following columns:
#' * `series_name` (`chr`)
#' * `coicop` (`chr`)
#' * `value` (`num`)
#' * `year` (`num`)
#' * `month` (`num`)
#'
#' @param country ISO 3166-1 alpha-2 (2 digit) country code.
#' @param level COICOP level. Default value is 2. Possible values are 1-3.
#' @param start_year Year of start date. Default value is NULL.
#' @param start_month Month of start date. Default value is NULL.
#' @param end_year Year of end date. Default value is NULL.
#' @param end_month Month of end date. Default value is NULL.
#' @returns A `data.table` object.
#' @seealso [load_weights()]
#' @examples
#' dt_cpi <- load_cpi("FR")
#' dt_cpi <- load_cpi("DE")
#' dt_cpi <- load_cpi("IT", level = 1, start_year = 2020, end_year = 2022)
#' dt_cpi <- load_cpi("ES", level = 1, end_year = 2023)
#'
#' @importFrom dplyr %>%
#' @export
load_cpi <- function(country, level = 2,
                     start_year = NULL, start_month = NULL,
                     end_year = NULL, end_month = NULL) {
  if (level < 1 | 3 < level) {
    stop("COICOP level must be 1, 2 or 3.")
  }

  # Determine specified time period
  dates <- get_start_end_dates(
    start_year, start_month,
    end_year, end_month
  )

  # Download dataset
  rdbnomics::rdb("Eurostat", "prc_hicp_midx",
    mask = paste0("M.I05..", country)
  ) %>%
    # Select data in specified time period
    .[dates$start_date <= period & period <= dates$end_date] %>%
    # Filter to specified COICOP level
    select_coicop_level(level) %>%
    # Rearrange columns
    .[, .(series_name, coicop, value,
      year = lubridate::year(period),
      month = lubridate::month(period)
    )]
}

#' Download annual index weights data
#'
#' @description
#' `load_weights()` downloads annual index weights data from Eurostat's HICP (Harmonised Indices of Consumer Prices) database via DBnomics from a specified country. The user must specify the granularity of the product classification according to the COICOP nomenclature. A time period must also be specified.
#'
#' The data is formatted as `data.table` with following columns:
#' * `coicop` (`chr`)
#' * `weight` (`num`)
#' * `year` (`num`)
#'
#' @param country ISO 3166-1 alpha-2 (2 digit) country code.
#' @param level COICOP level. Default value is 2. Possible values are 1-3.
#' @param start_year Year of start date. Default value is NULL.
#' @param end_year Year of end date. Default value is NULL.
#' @returns A `data.table` object.
#' @seealso [load_cpi()]
#' @examples
#' dt_weights <- load_weights("FR")
#' dt_weights <- load_weights("DE", level = 1)
#' dt_weights <- load_weights("IT", level = 1, start_year = 2020, end_year = 2022)
#' dt_weights <- load_weights("ES", level = 1, end_year = 2023)
#'
#' @importFrom dplyr %>%
#' @export
load_weights <- function(country, level = 2,
                         start_year = NULL, end_year = NULL) {
  if (level < 1 | 3 < level) {
    stop("COICOP level must be 1, 2 or 3.")
  }

  # Determine specified time period
  dates <- get_start_end_dates(
    start_year,
    start_month = 1,
    end_year, end_month = 12
  )

  rdbnomics::rdb("Eurostat", "prc_hicp_inw",
    mask = paste0("A..", country)
  ) %>%
    # Select data in specified time period
    .[dates$start_date <= period & period <= dates$end_date] %>%
    # Filter to specified COICOP level
    select_coicop_level(level) %>%
    # Rearrange columns
    # We don't pick series_name since it's annual (redundant) and has no COICOP code
    .[, .(coicop,
      weight = value,
      year = lubridate::year(period)
    )]
}

#' Download HBS (Household Budget Survey) data
#'
#' @description
#' `load_hbs()` downloads HBS data from Eurostat's databases via DBnomics from a specified country. The user must specify the granularity of the product classification according to the COICOP nomenclature. A time period and a demographic category must also be specified.
#'
#' Currently, the only dataset with level 3 COICOP data is from France in 2017 by income decile produced by INSEE. To access it, you must call `load_hbs("FR", category = "income", level = 3, start_year = 2017)`.
#'
#' @details
#' Given the same parameters (except category),
#'
#' The data is formatted as `data.table` with following columns:
#' * `series_name` (`chr`)
#' * `coicop` (`chr`)
#' * `year` (`num`)
#' * `category` (`chr`)
#' * `consumption` (`num`)
#'
#' @param country ISO 3166-1 alpha-2 (2 digit) country code.
#' @param category HBS data by category: `"income"`, `"age"`, `"urban"`
#' @param level COICOP level. Default value is 2. Possible values are 1-3.
#' @param start_year Year of start date. Default value is NULL.
#' @param end_year Year of end date. Default value is NULL.
#' @returns A `data.table` object.
#' @examples
#' dt_hbs <- load_hbs("FR", "income")
#' dt_hbs <- load_hbs("DE", "age", level = 1)
#' dt_hbs <- load_hbs("IT", "urban", start_year = 2020, end_year = 2022)
#' dt_hbs <- load_hbs("ES", "income", level = 1, end_year = 2023)
#'
#' @importFrom data.table :=
#' @importFrom dplyr %>%
#' @export
load_hbs <- function(country, category, level = 2,
                     start_year = NULL, end_year = NULL) {
  if (level < 1 | 3 < level) {
    stop("COICOP level must be 1, 2 or 3.")
  }

  # Determine specified time period
  dates <- get_start_end_dates(
    start_year,
    start_month = 1,
    end_year, end_month = 12
  )

  if (country == "FR" & level == 3 & category == "income") {
    # The short-circuiting version of | is ||
    if (is.null(start_year) || start_year < 2017) {
      stop("French HBS level 3 COICOP data only exists for 2017.")
    }

    # Import INSEE's 2017 HBS data post-correction.
    # INSEE dataset 4648335 TF106

    # Construct file path to the CSV file
    dt_hbs_file_path <- system.file("extdata", "TF106_3digit.csv",
      package = "inflationinequality"
    )

    # Read CSV file with specified locale settings and column types
    dt_hbs <- data.table::as.data.table(
      readr::read_csv2(dt_hbs_file_path,
        # French locale
        locale = readr::locale(
          encoding = "latin1",
          decimal_mark = ",",
          grouping_mark = " "
        ),
        col_types = readr::cols(
          "IDENT" = readr::col_character(),
          "Regroupement" = readr::col_character(),
          "Ensemble" = readr::col_number(),
          "Décile1" = readr::col_number(),
          "Décile2" = readr::col_number(),
          "Décile3" = readr::col_number(),
          "Décile4" = readr::col_number(),
          "Décile5" = readr::col_number(),
          "Décile6" = readr::col_number(),
          "Décile7" = readr::col_number(),
          "Décile8" = readr::col_number(),
          "Décile9" = readr::col_number(),
          "Décile10" = readr::col_number()
        )
      )
    )

    # Melt wide format to long format, and rearrange columns
    dt_hbs <- dt_hbs %>%
      data.table::melt(
        id.vars = c("IDENT", "Regroupement"), variable.name = "quantile",
        value.name = "consumption"
      ) %>%
      # Rearrange columns and remove "Ensemble" (Total) values
      .[quantile != "Ensemble", .(
        series_name = Regroupement,
        coicop = IDENT,
        year = 2017,
        category = quantile,
        consumption
      )] %>%
      select_coicop_level(level)
  } else if (level < 3) {
    data_code <- switch(category,
      "income" = "hbs_str_t223",
      "age" = "hbs_str_t225",
      "urban" = "hbs_str_t226"
    )
    rdbnomics::rdb("Eurostat", data_code,
      mask = paste0("A.PM...", country)
    ) %>%
      # Select data in specified time period
      .[dates$start_date <= period & period <= dates$end_date] %>%
      # Filter to specified COICOP level
      select_coicop_level(level) %>%
      # Rearrange columns
      # We don't pick series_name since it's annual (redundant) and has no COICOP code
      .[, .(series_name, coicop,
        year = lubridate::year(period),
        category = switch(category,
          "income" = quantile,
          "age" = age,
          "urban" = deg_urb
        ),
        consumption = value
      )]
  } else {
    stop("HBS level 3 COICOP data does not exist for the specified parameters.")
  }
}

# Select COICOP level
# (This is a helper or private function, it should not be public!)
#' @importFrom data.table :=
#' @importFrom dplyr %>%
select_coicop_level <- function(.dt, level) {
  # Select rows with "CP" prefix followed by numeric values or only numeric values
  .dt[grepl("^CP\\d+|^\\d+$", coicop), ] %>%
    # Remove "CP" substring
    .[grepl("^CP", coicop), coicop := sub("^CP", "", coicop)] %>%
    # Select specified COICOP level
    .[nchar(coicop) == level + 1, ]
}

get_start_end_dates <- function(start_year = NULL, start_month = NULL,
                                end_year = NULL, end_month = NULL) {
  # Set start and end months to 01 and 12 if only year is provided
  if (is.null(start_month)) start_month <- 1
  if (is.null(end_month)) end_month <- 12

  # Create start and end date strings
  start_date <- if (!is.null(start_year)) {
    sprintf("%04d-%02d-01", start_year, start_month)
  } else {
    "0001-01-01"
  }

  end_date <- if (!is.null(end_year)) {
    sprintf("%04d-%02d-01", end_year, end_month)
  } else {
    Sys.Date()
  }

  # Check if end date is before start date
  if (end_date < start_date) {
    stop("End date cannot be before start date.")
  }

  return(list(start_date = start_date, end_date = end_date))
}
