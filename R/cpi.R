new_cpi <- function(dt = data.table::data.table(),
                    dt_basket = data.table::data.table(),
                    country = character(), level = numeric()) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(data.table::is.data.table(dt_basket))
  stopifnot(is.character(country))
  stopifnot(is.numeric(level))

  start_year = dt[, min(year)]
  start_month = dt[year == start_year, min(month)]
  end_year = dt[, max(year)]
  end_month = dt[year == end_year, max(month)]

  structure(list(
    dt = dt,
    dt_basket = dt_basket,
    country = country,
    level = level,
    start_year = start_year,
    start_month = start_month,
    end_year = end_year,
    end_month = end_month),
    class = "cpi"
  )
}

validate_cpi <- function(cpi) {
  required_columns <- c("series_name", "coicop", "value", "year", "month")
  missing_columns <- setdiff(required_columns, names(cpi$dt))

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from 'dt': ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (cpi$level < 1 | 3 < cpi$level) {
    stop("COICOP level must be 1, 2 or 3.")
  }

  cpi$dt <- cpi$dt[!is.na(year) | !is.na(month) | !is.na(coicop)]
  cpi$dt_basket <- cpi$dt_basket[!is.na(year) | !is.na(month)]

  data.table::setkey(cpi$dt, year, month, coicop)
  data.table::setkey(cpi$dt_basket, year, month)

  cpi$dt[, value := pmax(value, 1e-6, na.rm = TRUE)]
  cpi$dt_basket[, value := pmax(value, 1e-6, na.rm = TRUE)]

  timepoints <- unique(cpi$dt[, .(year, month)])
  basket_timepoints <- cpi$dt_basket[, .(year, month)]

  if (!setequal(timepoints, basket_timepoints)) {
    message("dt_basket does not have the same timepoints as dt!")
  }

  cpi
}

#' Constructor for a CPI data object
#'
#' @description
#' `cpi()` constructs an object of class `"cpi"` which contains monthly CPI
#' (Consumer Price Index) data of products categorized by their COICOP code from
#' a single country.
#'
#' @param dt a `data.table` object (see details).
#' @param dt_basket a `data.table` object (see details).
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one
#'   country at a time is accepted.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is
#'   level 1 (Division), "012" is level 2 (Group), and "0111" is level 3
#'   (Class).
#'
#' @details
#' The component `dt` contains the following columns:
#'
#' - `series_name`: identifier for the data series.
#' - `coicop`: COICOP (Classification of Individual Consumption by Purpose)
#' code.
#' - `value`: price of item category at the specified time.
#' - `year`: year of the data series.
#' - `month`: month of the data series.
#'
#' The component `dt_basket` has the following columns:
#'
#' - `series_name`: identifier for the data series.
#' - `value`: price of item category at the specified time.
#' - `year`: year of the data series.
#' - `month`: month of the data series.
#'
#' For each (year, month) pair in `dt`, there should be a corresponding
#' (year, month) pair in `dt_basket`.
#'
#' @returns An object of class `"cpi"` is a list containing the following components:
#'
#' - `dt`: a `data.table` object, each row in `dt` represents a single price
#' observation for a specific item category at a particular point in time (see
#' details).
#' - `dt_basket`: a `data.table` object, each row in `dt` represents a single
#' price observation for the entire price basket at a particular point in time
#' (see details).
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `start_month`: first month of data.
#' - `end_year`: last year of data.
#' - `end_month`: last month of data.
#'
#' @export
cpi <- function(dt = data.table::data.table(),
                dt_basket = data.table::data.table(),
                country = character(), level = numeric()) {
  validate_cpi(new_cpi(dt, dt_basket, country, level))
}

#' Search for missing CPI data
#'
#' @description
#' `get_missing_cpi_tuples()` searches for the years and months where a product
#' does not have CPI data.
#'
#' @param cpi an object of class `"cpi"`.
#'
#' @returns a `data.table` object with the following columns:
#'
#' - `coicop`: COICOP code.
#' - `year`: year.
#' - `month`: month.
#'
#' @examples
#' # Download CPI data from Eurostat
#' cpi <- load_cpi("FR")
#'
#' # Find missing data
#' missing_data <- get_missing_cpi_tuples(cpi)
#'
#' if (length(missing_data) > 0) {
#'   message("There is data missing in French CPI data!")
#' } else {
#'   message("There is no data missing in French CPI data.")
#' }
#'
#' @export
get_missing_cpi_tuples <- function(cpi) {
  UseMethod("get_missing_cpi_tuples")
}

#' @exportS3Method
get_missing_cpi_tuples.cpi <- function(cpi) {
  # Create all possible combinations up to the max date
  all_tuples <- data.table::CJ(coicop = cpi$dt[, unique(coicop)],
                               year = (cpi$start_year):(cpi$end_year),
                               month = 1:12)

  # Filter out future dates
  all_tuples <- all_tuples[year < cpi$end_year
                           | (year == cpi$end_year & month <= cpi$end_month)]

  # Get existing tuples
  existing_tuples <- unique(cpi$dt[, .(coicop, year, month)])

  # Find missing tuples
  missing_tuples <- all_tuples[!existing_tuples, on = .(coicop, year, month)]

  return(missing_tuples)
}

#' Synthesize missing CPI data
#'
#' @description
#' `correct_cpi()` fills in missing Consumer Price Index (CPI) data for products
#' at COICOP level 2 and higher. It uses the annual growth rates of the
#' corresponding level 1 COICOP category to estimate missing values. This
#' function is particularly useful for handling incomplete CPI datasets.
#'
#' @param cpi An object of class `"cpi"` containing the original CPI data.
#'
#' @details
#' The function performs the following steps:
#'
#' 1. Identifies missing CPI data points.
#' 2. Retrieves level 1 COICOP data from Eurostat.
#' 3. Calculates annual growth rates for level 1 categories.
#' 4. Uses these growth rates to backfill missing data for higher level
#' categories.
#'
#' This method assumes that subcategories within a COICOP group follow similar
#' price trends as their parent category.
#'
#' @return An object of class `"cpi"` with synthesized data points added.
#'
#' @note
#' This function only works for COICOP levels 2 and higher. Attempting to use it
#' on level 1 data will result in an error.
#'
#' @seealso
#' [cpi()] for creating CPI objects and [load_cpi()] for loading CPI data from
#' Eurostat
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_cpi' is a cpi object with missing data
#' corrected_cpi <- correct_cpi(my_cpi)
#' }
#'
#' @export
correct_cpi <- function(cpi) {
  UseMethod("correct_cpi")
}

#' @exportS3Method
correct_cpi.cpi <- function(cpi) {
  if (cpi$level < 2) {
    stop("Correcting CPI data only works for level 2 COICOP and higher.")
  }

  missing_cpi_tuples <- get_missing_cpi_tuples(cpi)
  missing_coicops <- missing_cpi_tuples[, unique(coicop)]
  level1_coicops <- unique(substr(missing_coicops, 1, 2))

  # Pick specific COICOPs for more efficiency
  level1_cpi <- load_cpi(cpi$country, level = 1,
                         start_year = missing_cpi_tuples[, min(year)],
                         # We need an extra year...
                         end_year = missing_cpi_tuples[, max(year)] + 1)

  ### Calculate annual growth rates
  # Ensure the data is sorted correctly first
  data.table::setorder(cpi$dt, coicop, year, month)

  # Create a year-month column for easier shifting
  cpi$dt[, yearmonth := as.Date(paste(year, month, "01", sep = "-"))]

  # Now calculate the lagged value
  cpi$dt[, lagging_value := shift(value, n = 12, type = "lag"), by = coicop]

  # Calculate the growth rate
  # The growth rate g_t follows the equation
  # x_(t+12) = x_t * (1 + (g_t / 100))
  # Also, growth_rate is expressed in percentages
  cpi$dt[, growth_rate := (value / lagging_value - 1) * 100]
  ###

  # Ensure data is sorted
  data.table::setorder(cpi$dt, coicop, year, month)
  data.table::setorder(level1_cpi$dt, coicop, year, month)

  # Create date columns
  cpi$dt[, date := as.Date(paste(year, month, "01", sep = "-"))]
  level1_cpi$dt[, date := as.Date(paste(year, month, "01", sep = "-"))]

  # Function to backfill for a single COICOP
  backfill_single_coicop <- function(coicop_level2) {
    coicop_level1 <- substr(coicop_level2, 1, 2)

    # Get the data for this COICOP and its level 1 counterpart
    coicop_data <- cpi$dt[coicop == coicop_level2, ]
    level1_data <- level1_cpi$dt[coicop == coicop_level1, ]

    # Ensure data is sorted
    data.table::setorder(coicop_data, date)
    data.table::setorder(level1_data, date)

    # Find the earliest date in coicop_data
    earliest_coicop_date <- min(coicop_data$date)

    # Get level1 data for dates before earliest_coicop_date
    backfill_data <- level1_data[date < (earliest_coicop_date + lubridate::years(1)), ]

    result <- data.table::data.table(
      series_name = character(),
      coicop = character(),
      year = numeric(),
      month = numeric(),
      date = lubridate::Date(),
      value = numeric()
    )

    # We iterate for each next_date, which is a date that is missing CPI data
    next_date <- coicop_data[, min(date) - months(1)]
    while (next_date %in% level1_data[, date]) {
      previous_date <- next_date + lubridate::years(1)
      growth_rate <- backfill_data[date == previous_date, growth_rate]

      # We take the previous value from level 2 COICOP data first,
      # then we take it from the synthesized data
      previous_value <- if (previous_date %in% coicop_data[, date]) {
        coicop_data[date == previous_date, value]
      } else {
        result[date == previous_date, value]
      }

      next_row <- data.table::data.table(
        series_name = NA_character_,
        coicop = coicop_level2,
        year = lubridate::year(next_date),
        month = lubridate::month(next_date),
        date = next_date,
        value = previous_value / (1 + growth_rate / 100)
      )

      result <- data.table::rbindlist(list(result, next_row))
      next_date <- next_date - months(1)
    }

    return(result)
  }

  # Apply backfilling to all missing COICOPs
  backfilled_list <- lapply(missing_coicops, backfill_single_coicop)

  # Combine all backfilled data
  all_backfilled <- data.table::rbindlist(backfilled_list)

  # Combine with original data
  result <- data.table::rbindlist(list(
    cpi$dt[, .(series_name, coicop, year, month, value)],  # Add growth_rate column to original data
    all_backfilled[, .(series_name, coicop, year, month, value)]
  ), use.names = TRUE, fill = TRUE)

  return(cpi(result, dt_basket, country, level))
}
