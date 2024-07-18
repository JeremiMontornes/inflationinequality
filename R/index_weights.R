new_index_weights <- function(dt = data.table::data.table(),
                    country = character(), level = numeric()) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(is.character(country))
  stopifnot(is.numeric(level))

  start_year = dt[, min(year)]
  end_year = dt[, max(year)]

  structure(list(
    dt = dt,
    country = country,
    level = level,
    start_year = start_year,
    end_year = end_year),
    class = "index_weights"
  )
}

validate_index_weights <- function(index_weights) {
  required_columns <- c("coicop", "weight", "year")
  missing_columns <- setdiff(required_columns, names(index_weights$dt))

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from 'dt': ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (index_weights$level < 1 | 3 < index_weights$level) {
    stop("COICOP level must be 1, 2 or 3.")
  }

  index_weights
}

#' Constructor for a CPI weight data object
#'
#' @description `index_weights()` constructs an object of class
#' `"index_weights"` which contains annual CPI (Consumer Price Index) weight
#' data of products categorized by their COICOP code from a single country.
#'
#' @param dt a `data.table` object (see details).
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one
#'   country at a time is accepted.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is
#'   level 1 (Division), "012" is level 2 (Group), and "0111" is level 3
#'   (Class).
#'
#' @details The component `dt` has the following columns:
#'
#' - `coicop`: COICOP code.
#' - `weight`: weight of item category at specified time.
#' - `year`: year of data series.
#'
#' The weights of all item categories in a given year must be normalized to
#' 100.
#'
#' @returns An object of class `"index_weights"` is a list containing the
#' following components:
#'
#' - `dt`: a `data.table` object (see details), each row in `dt` represents the
#' weighting for a specific item category in the CPI at a particular point in
#' time (see details).
#' observation for a specific item category
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `end_year`: last year of data.
#'
#' @export
index_weights <- function(dt = data.table::data.table(),
                country = character(), level = numeric()) {
  validate_index_weights(new_index_weights(dt, country, level))
}
