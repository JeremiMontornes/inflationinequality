# BdF-specific
options(rdbnomics.use_readLines = TRUE)

hbs_dataset_data <- list(
  income = "hbs_str_t223",
  age = "hbs_str_t225",
  urban = "hbs_str_t226"
)

# Map for Eurostat categories
category_data <- list(
  income = list(
    old_names = c("QU1", "QU2", "QU3", "QU4", "QU5"),
    categories = c("First quintile", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile")
  ),
  age = list(
    old_names = c("Y_LT30", "Y30-44", "Y45-59", "Y_GE60"),
    categories = c("Less than 30 years", "From 30 to 44 years", "From 45 to 59 years", "60 years or over")
  ),
  urban = list(
    old_names = c("DEG3", "DEG2", "DEG1"),
    categories = c("Rural areas", "Towns and suburbs", "Cities")
  )
)

#' Downloads monthly CPI (Consumer Price Index) data
#'
#' @description
#' `load_cpi()` downloads monthly CPI data from Eurostat's HICP (Harmonised
#' Indices of Consumer Prices) database via DBnomics from a specified country.
#'
#' @inheritParams load_index_weights
#' @param start_month month of start date.
#' @param end_month month of end date.
#'
#' @details
#' It's possible that some datasets do not contain all available COICOP codes on
#' particular years.
#'
#' @returns An object of class `"cpi"` (see [cpi()]).
#'
#' @examples
#' # Download all available French CPI data
#' cpi <- load_cpi("FR")
#'
#' # Download all available German CPI data
#' cpi <- load_cpi("DE")
#'
#' # Download Italian CPI data at COICOP level 1 from 2020 to 2022
#' cpi <- load_cpi("IT", level = 1, start_year = 2020, end_year = 2022)
#'
#' # Download Spanish CPI data at COICOP level 1 up to September 2023
#' cpi <- load_cpi("ES", level = 1, end_year = 2023, end_month = 9)
#'
#' # Access the data.table component
#' dt_cpi <- cpi$dt
#'
#' @seealso [cpi()]
#'
#' @importFrom dplyr %>%
#' @export
load_cpi <- function(country, level = 2,
                     start_year = NULL, start_month = NULL,
                     end_year = NULL, end_month = NULL) {
  # Input validation
  if (!is.character(country) || nchar(country) != 2) {
    stop("Country must be a 2-character ISO code")
  }
  if (!is.numeric(level) || !level %in% 1:3) {
    stop("Level must be an integer between 1 and 3")
  }

  # Determine specified time period
  dates <- get_start_end_dates(
    start_year, start_month,
    end_year, end_month
  )

  dataset_code <- resolve_hicp_cpi_dataset()

  # hicp::data expects YYYY-MM date ranges for monthly datasets
  date_range <- c(
    format(as.Date(dates$start_date), "%Y-%m"),
    format(as.Date(dates$end_date), "%Y-%m")
  )

  # Download dataset from Eurostat via hicp package
  dt_raw <- hicp::data(
    id = dataset_code,
    filters = list(freq = "M", geo = country, unit = "I15"),
    date.range = date_range
  )

  # Keep COICOP item codes only; the all-items aggregate is handled separately in dt_basket.
  dt <- dt_raw[
    grepl("^CP\\d+$", coicop18),
    .(
      series_name = paste0(dataset_code, ".", unit, ".", coicop18, ".", geo),
      coicop = coicop18,
      value = values,
      period = as.Date(paste0(time, "-01"))
    )
  ] %>%
    select_coicop_level(level) %>%
    .[, .(
      series_name,
      coicop,
      value,
      year = lubridate::year(period),
      month = lubridate::month(period)
    )]

  # Download price basket (all-items aggregate)
  # New ECOICOPv2 datasets may expose the all-items aggregate as "TOTAL"
  # instead of legacy "AP". Try TOTAL first, then fallback to AP.
  dt_basket_raw <- tryCatch(
    hicp::data(
      id = dataset_code,
      filters = list(freq = "M", geo = country, unit = "I15", coicop18 = "TOTAL"),
      date.range = date_range
    ),
    error = function(e) {
      hicp::data(
        id = dataset_code,
        filters = list(freq = "M", geo = country, unit = "I15", coicop18 = "AP"),
        date.range = date_range
      )
    }
  )

  dt_basket <- dt_basket_raw[, .(
    series_name = paste0(dataset_code, ".", unit, ".", coicop18, ".", geo),
    value = values,
    year = as.integer(substr(time, 1, 4)),
    month = as.integer(substr(time, 6, 7))
  )]

  return(cpi(dt, dt_basket, country, level))
}

resolve_hicp_cpi_dataset <- function() {
  # Prefer the current ecoicopv2 monthly index dataset, with fallback for compatibility.
  candidates <- c("prc_hicp_minr", "prc_hicp_midx")
  available <- tryCatch(hicp::datasets(), error = function(e) data.table::data.table())

  for (code in candidates) {
    if ("code" %in% names(available) && any(available[["code"]] %chin% code)) {
      return(code)
    }
  }

  stop("No available HICP CPI dataset found (tried: prc_hicp_minr, prc_hicp_midx).")
}

#' Downloads annual index weights data
#'
#' @description
#' `load_index_weights()` downloads annual index weights data from Eurostat's
#' HICP (Harmonised Indices of Consumer Prices) database via DBnomics from a
#' specified country.
#'
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one
#'   country at a time is accepted.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is
#'   level 1 and "012" is level 2. Default value is 2.
#' @param start_year year of start date.
#' @param end_year year of end date.
#'
#' @returns An object of class `"index_weights"`.
#'
#' @examples
#' # Download all available French index weights data
#' weights <- load_index_weights("FR")
#'
#' # Download German index weights data at COICOP level 1
#' weights <- load_index_weights("DE", level = 1)
#'
#' # Download Italian index weights data at COICOP level 1 from 2020 to 2022
#' weights <- load_index_weights("IT", level = 1, start_year = 2020, end_year = 2022)
#'
#' # Download Spanish index weights data at COICOP level 1 up to 2023
#' weights <- load_index_weights("ES", level = 1, end_year = 2023)
#'
#' @seealso [index_weights()]
#'
#' @importFrom dplyr %>%
#' @export
load_index_weights <- function(country, level = 2,
                               start_year = NULL, end_year = NULL) {
  # Input validation
  if (!is.character(country) || nchar(country) != 2) {
    stop("Country must be a 2-character ISO code")
  }
  if (!is.numeric(level) || !level %in% 1:3) {
    stop("Level must be an integer between 1 and 3")
  }

  # Determine specified time period
  dates <- get_start_end_dates(
    start_year,
    start_month = 1,
    end_year, end_month = 12
  )

  dataset_code <- resolve_hicp_weights_dataset()

  date_start_year <- as.integer(substr(dates$start_date, 1, 4))
  date_end_year <- as.integer(substr(dates$end_date, 1, 4))

  # Download annual weights directly from Eurostat via hicp package.
  dt_raw <- hicp::data(
    id = dataset_code,
    filters = list(geo = country)
  )

  coicop_col <- if ("coicop18" %in% names(dt_raw)) "coicop18" else "coicop"
  if (!(coicop_col %in% names(dt_raw))) {
    stop(sprintf("No COICOP column found in dataset '%s'.", dataset_code))
  }

  dt <- dt_raw[
    # Keep only item COICOP codes
    grepl("^CP\\d+$", get(coicop_col))
      # If statinfo exists (new dataset), retain index weights rows
      & (!("statinfo" %in% names(dt_raw)) | statinfo == "IW"),
    .(
      coicop = get(coicop_col),
      weight = values,
      year = as.integer(substr(time, 1, 4))
    )
  ] %>%
    .[data.table::between(year, date_start_year, date_end_year)] %>%
    select_coicop_level(level) %>%
    .[, .(coicop, weight, year)]

  if (!is.null(end_year)) {
    if (nrow(dt) == 0) {
      warning(
        sprintf(
          "No index weights found in '%s' for requested period (start_year=%s, end_year=%s).",
          dataset_code,
          if (is.null(start_year)) "NULL" else start_year,
          end_year
        )
      )
    } else if (max(dt$year, na.rm = TRUE) < end_year) {
      warning(
        sprintf(
          "Requested end_year=%s but latest available year in '%s' is %s.",
          end_year, dataset_code, max(dt$year, na.rm = TRUE)
        )
      )
    }
  }

  return(index_weights(dt, country, level, base_total = 1000))
}

resolve_hicp_weights_dataset <- function() {
  # Prefer the new ecoicopv2 dataset when available; fallback keeps backward compatibility.
  candidates <- c("prc_hicp_iw", "prc_hicp_inw")
  available <- tryCatch(hicp::datasets(), error = function(e) data.table::data.table())
  for (code in candidates) {
    if ("code" %in% names(available) && any(available[["code"]] %chin% code)) {
      return(code)
    }
  }

  stop("No available HICP weights dataset found (tried: prc_hicp_iw, prc_hicp_inw).")
}

#' Downloads HBS (Household Budget Survey) data
#'
#' @description
#' `load_hbs()` downloads HBS data from Eurostat's databases via DBnomics from a
#' specified country.
#'
#' @details
#' Eurostat only provides HBS data at level 1 and level 2 COICOP.
#'
#' @inheritParams load_index_weights
#' @param category HBS data by category: `"income"`, `"age"`, `"urban"`.
#'
#' @returns An object of class `"hbs"`.
#'
#' @examples
#' # Download French HBS data by income
#' hbs <- load_hbs("FR", "income")
#'
#' # Download German HBS data by age at COICOP level 1
#' hbs <- load_hbs("DE", "age", level = 1)
#'
#' # Download Italian HBS data by urban/rural from 2020 to 2022
#' hbs <- load_hbs("IT", "urban", start_year = 2020, end_year = 2022)
#'
#' # Download Spanish HBS data by income at COICOP level 1 up to 2023
#' hbs <- load_hbs("ES", "income", level = 1, end_year = 2023)
#'
#' # Access the data.table component
#' dt_hbs <- hbs$dt
#'
#' @importFrom data.table :=
#' @importFrom dplyr %>%
#' @export
load_hbs <- function(country, category, level = 2,
                     start_year = NULL, end_year = NULL) {
  # Input validation
  if (!is.character(country) || nchar(country) != 2) {
    stop("Country must be a 2-character ISO code")
  }
  if (!category %in% c("income", "age", "urban")) {
    stop("Category must be one of 'income', 'age', or 'urban'")
  }
  if (!is.numeric(level) || !level %in% 1:3) {
    stop("Level must be an integer between 1 and 3")
  }

  # Determine specified time period
  dates <- get_start_end_dates(
    start_year,
    start_month = 1,
    end_year, end_month = 12
  )
  category_input <- category

    # Determine Eurostat dataset code
    dataset_code <- hbs_dataset_data[[category]]
    old_names <- category_data[[category]]$old_names
    categories <- category_data[[category]]$categories

    # Download dataset
    dt <- rdbnomics::rdb("Eurostat", dataset_code,
      dimensions = list(freq = "A", geo = country, unit = "PM")
    ) %>%
      # Select data in specified time period
      .[data.table::between(period, dates$start_date, dates$end_date)] %>%
      # Filter to specified COICOP level
      select_coicop_level(level) %>%
      # Rearrange columns
      # We don't pick series_name since it's annual (redundant) and has no
      # COICOP code
      .[, {
        if (category_input == "income") {
          category_raw <- if ("quant_inc" %in% names(.SD)) quant_inc else if ("quantile" %in% names(.SD)) quantile else if ("incgrp" %in% names(.SD)) incgrp else rep(NA_character_, .N)
        } else if (category_input == "age") {
          category_raw <- if ("age" %in% names(.SD)) age else if ("age_cl" %in% names(.SD)) age_cl else rep(NA_character_, .N)
        } else {
          category_raw <- if ("deg_urb" %in% names(.SD)) deg_urb else if ("degree_urb" %in% names(.SD)) degree_urb else rep(NA_character_, .N)
        }
        .(series_name, coicop,
          year = lubridate::year(period),
          category = category_raw,
          consumption = value)
      }] %>%
      .[, if (all(is.na(category))) {
        stop(
          sprintf(
            "Could not identify the DBnomics category column for category '%s'.",
            category_input
          )
        )
      } else .SD] %>%
      .[category %in% old_names,
        category := categories[match(category, old_names)]] %>%
      .[category %in% categories]

    # Download dataset
    dt_total <-
      rdbnomics::rdb(
        "Eurostat", "hbs_str_t211",
        dimensions = list(freq = "A", geo = country, unit = "PM")
    ) %>%
      # Select data in specified time period
      .[dates$start_date <= period & period <= dates$end_date] %>%
      # Filter to specified COICOP level
      select_coicop_level(level) %>%
      # Rearrange columns
      # We don't pick series_name since it's annual (redundant) and has no COICOP code
      .[, .(coicop,
            year = lubridate::year(period),
            total_consumption = value
      )]

    return(hbs(dt, dt_total, country, category, categories, level))
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
    .[nchar(coicop) <= level + 1, ]
}

produce_coicop_mask <- function(dataset_code, prefix, suffix, level) {
  dimensions <- rdbnomics::rdb_dimensions("Eurostat", dataset_code,
                                          mask = paste0(prefix, "..", suffix)
  )
  coicop_codes <- dimensions[[1]][[1]]$coicop$coicop
  selected <-
    coicop_codes[
      grepl("^CP\\d+", coicop_codes)
      & nchar(coicop_codes) <= level + 3
      & coicop_codes != "CP00" # Ignore CP00
    ]

  paste(selected, collapse = "+")
}

produce_filtered_mask <- function(dataset_code, prefix, suffix, level) {
  coicop_mask <- produce_coicop_mask(dataset_code, prefix, suffix, level)
  paste0(prefix, ".", coicop_mask, ".", suffix)
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
