# BdF-specific
options(rdbnomics.use_readLines = TRUE)

#' Downloads monthly CPI (Consumer Price Index) data
#'
#' @description
#' `load_cpi()` downloads monthly CPI data from Eurostat's HICP (Harmonised Indices of Consumer Prices) database via DBnomics from a specified country.
#'
#' @details
#' It's possible that some datasets do not contain all available COICOP codes on particular years.
#'
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one country at a time is accepted.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is level 1 and "012" is level 2.
#' @param start_year year of start date.
#' @param start_month month of start date.
#' @param end_year year of end date.
#' @param end_month month of end date.
#'
#' @returns An object of class `"cpi"` is a list containing the following components:
#' \item{dt}{a `data.table` object (see below).}
#' \item{country}{2-digit country code (see ISO 3166-1 alpha-2).}
#' \item{level}{COICOP level.}
#' \item{start_year}{first year of data.}
#' \item{start_month}{first month of data.}
#' \item{end_year}{last year of data}
#' \item{end_month}{last month of data.}
#'
#' The component `dt` has the following columns:
#' \item{series_name}{identifier for the data series.}
#' \item{coicop}{COICOP code.}
#' \item{value}{price of item category at specified time.}
#' \item{year}{year of data series.}
#' \item{month}{month of data series.}
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
#' @seealso [load_index_weights()]
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

  dataset_code <- "prc_hicp_midx"
  mask_prefix <- "M.I05"
  filtered_mask <- produce_filtered_mask(dataset_code, mask_prefix, country, level)

  # Download dataset
  dt <- rdbnomics::rdb("Eurostat", dataset_code,
    mask = filtered_mask
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

  return(structure(
    list(
      dt = dt,
      country = country,
      level = level,
      start_year = min(dt$year),
      start_month = 1,
      end_year = max(dt$year),
      end_month = max(dt[year == max(dt$year), month])
    ),
    class = "cpi"
  ))
}

#' Downloads annual index weights data
#'
#' @description
#' `load_index_weights()` downloads annual index weights data from Eurostat's HICP (Harmonised Indices of Consumer Prices) database via DBnomics from a specified country.
#'
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one country at a time is accepted.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is level 1 and "012" is level 2. Default value is 2.
#' @param start_year year of start date.
#' @param end_year year of end date.
#'
#' @returns An object of class `"index_weights"` is a list containing the following components:
#' \item{dt}{a `data.table` object (see below).}
#' \item{country}{2-digit country code (see ISO 3166-1 alpha-2).}
#' \item{level}{COICOP level.}
#' \item{start_year}{first year of data.}
#' \item{end_year}{last year of data.}
#'
#' The component `dt` has the following columns:
#' \item{coicop}{COICOP code.}
#' \item{weight}{weight of item category at specified time.}
#' \item{year}{year of data series.}
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
#' # Access the data.table component
#' dt_weights <- weights$dt
#'
#' @seealso [load_cpi()]
#'
#' @importFrom dplyr %>%
#' @export
load_index_weights <- function(country, level = 2,
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

  dataset_code <- "prc_hicp_inw"
  mask_prefix <- "A"
  filtered_mask <- produce_filtered_mask(dataset_code, mask_prefix, country, level)

  # Download dataset
  dt <- rdbnomics::rdb("Eurostat", dataset_code,
    mask = filtered_mask
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

  # return(new("AnnualTimeSeries",
  #            dt = dt,
  #            country = country,
  #            level = level,
  #            start_year = min(dt$year),
  #            end_year = max(dt$year)))

  return(structure(
    list(
      dt = dt,
      country = country,
      level = level,
      start_year = min(dt$year),
      end_year = max(dt$year)
    ),
    class = "index_weights"
  ))
}

#' Downloads HBS (Household Budget Survey) data
#'
#' @description
#' `load_hbs()` downloads HBS data from Eurostat's databases via DBnomics from a specified country.
#'
#' @details
#' Currently, the only dataset with level 3 COICOP data is from France in 2017 by income decile produced by INSEE. To access it, you must call `load_hbs("FR", category = "income", level = 3, start_year = 2017)`.
#'
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one country at a time is accepted.
#' @param category HBS data by category: `"income"`, `"age"`, `"urban"`.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is level 1 and "012" is level 2. Default value is 2.
#' @param start_year year of start date.
#' @param end_year year of end date.
#'
#' @returns An object of class `"hbs"` is a list containing the following components:
#' \item{dt}{a `data.table` object (see below).}
#' \item{country}{2-digit country code (see ISO 3166-1 alpha-2).}
#' \item{category}{HBS category: `"income"`, `"age"`, `"urban"`.}
#' \item{categories}{(Ordered) vector of category types, from lowest to highest.}
#' \item{level}{COICOP level.}
#' \item{start_year}{first year of data.}
#' \item{end_year}{last year of data.}
#'
#' The component `dt` has the following columns:
#' \item{series_name}{identifier for the data series.}
#' \item{coicop}{COICOP code.}
#' \item{year}{year of data series.}
#' \item{category}{category type within the specified HBS category.}
#' \item{consumption}{consumption value for the specified category and COICOP code.}
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
          .default = readr::col_number(),
          "IDENT" = readr::col_character(),
          "Regroupement" = readr::col_character()
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

    return(structure(
      list(
        dt = dt_hbs,
        country = country,
        category = category,
        categories = c("Decile1", "Decile2", "Decile3", "Decile4", "Decile5", "Decile6", "Decile7", "Decile8", "Decile9", "Decile10"),
        level = level,
        start_year = min(dt_hbs$year),
        end_year = max(dt_hbs$year)
      ),
      class = "hbs"
    ))
  } else if (level < 3) {
    dataset_code <- switch(category,
      "income" = "hbs_str_t223",
      "age" = "hbs_str_t225",
      "urban" = "hbs_str_t226",
      stop("Error: Invalid category. Please choose 'income', 'age', or 'urban'.")
    )

    mask_prefix <- "A.PM."
    filtered_mask <- produce_filtered_mask(dataset_code, mask_prefix, country, level)

    # Download dataset
    dt <- rdbnomics::rdb("Eurostat", dataset_code,
      mask = filtered_mask
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

    # I should change this, this is not clean
    if (category == "income") {
      old_names <- c("QUINTILE1", "QUINTILE2", "QUINTILE3", "QUINTILE4", "QUINTILE5")
      categories <- c("First quintile",
                     "Second quintile",
                     "Third quintile",
                     "Fourth quintile",
                     "Fifth quintile")
    } else if (category == "age") {
      old_names <- c("Y_LT30", "Y30-44", "Y45-59", "Y_GE60")
      categories <- c("Less than 30 years",
                     "From 30 to 44 years",
                     "From 45 to 59 years",
                     "60 years or over")
    } else if (category == "urban") {
      old_names <- c("DEG3", "DEG2", "DEG1")
      categories <- c("Rural areas",
                     "Towns and suburbs",
                     "Cities")
    }

    dt[category %in% old_names, category := categories[match(category, old_names)]]

    return(structure(
      list(
        dt = dt,
        country = country,
        category = category,
        categories = categories,
        level = level,
        start_year = min(dt$year),
        end_year = max(dt$year)
      ),
      class = "hbs"
    ))
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

produce_filtered_mask <- function(dataset_code, mask_prefix, country, level) {
  dimensions <- rdbnomics::rdb_dimensions("Eurostat", dataset_code,
    mask = paste0(mask_prefix, "..", country)
  )
  coicop_codes <- dimensions[[1]][[1]]$coicop$coicop
  selected <- coicop_codes[grepl("^CP\\d+", coicop_codes) & nchar(coicop_codes) == level + 3]
  coicop_mask <- paste(selected, collapse = "+")
  paste0(mask_prefix, ".", coicop_mask, ".", country)
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
