.onLoad <- function(libname, pkgname) {
  calculate_contributions <<- memoise::memoise(calculate_contributions)
}

#' Calculates inflation rates
#'
#' @description
#' `calculate_inflation()` computes inflation rates for different categories over time using the contributions calculated by `calculate_contributions()`.
#'
#' @details
#' The function performs the following key operations:
#' 1. Calls `calculate_contributions()` to get the inflation contributions.
#' 2. Sums up the total inflation for each category, year, and month.
#'
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one country at a time is accepted.
#' @param category Category for which to calculate inflation: "income", "age", or "urban".
#' @param level COICOP level. Possible values are 1-3. For example, "01" is level 1 and "012" is level 2. Default value is 2.
#' @param start_year year of start date.
#' @param start_month month of start date.
#' @param end_year year of end date.
#' @param end_month month of end date.
#'
#' @returns An object of class `"inflation"` is a list containing the following components:
#' \item{dt}{a `data.table` object (see below).}
#' \item{country}{2-digit country code (see ISO 3166-1 alpha-2).}
#' \item{category}{Category for which inflation was calculated: "income", "age", or "urban".}
#' \item{categories}{(Ordered) vector of category types, from lowest to highest.}
#' \item{level}{COICOP level.}
#' \item{start_year}{first year of data.}
#' \item{start_month}{first month of data.}
#' \item{end_year}{last year of data.}
#' \item{end_month}{last month of data.}
#'
#' The component `dt` has the following columns:
#' \item{year}{year of the inflation rate.}
#' \item{month}{month of the inflation rate.}
#' \item{category}{demographic category (e.g., income group, age group).}
#' \item{inflation}{calculated inflation rate.}
#'
#' @examples
#' # Calculate inflation rates for France, income category, COICOP level 2, from 2010 to 2020
#' france_inflation <- calculate_inflation("FR", "income", level = 2, start_year = 2010, end_year = 2020)
#'
#' # Access the data.table component
#' dt_inflation <- france_inflation$dt
#'
#' # Get inflation rate for a specific year, month, and category
#' specific_inflation <- dt_inflation[year == 2015 & month == 6 & category == "First quintile", inflation]
#'
#' @seealso [calculate_contributions()]
#'
#' @importFrom data.table :=
#' @export
calculate_inflation <- function(country, category, level = 2,
                                start_year = NULL, start_month = NULL,
                                end_year = NULL, end_month = NULL) {
  contributions <- calculate_contributions(country, category,
    level = level,
    start_year = start_year, start_month = start_month,
    end_year = end_year, end_month = end_month
  )
  dt_inflation <- contributions$dt[, .(inflation = sum(contribution)), by = .(year, month, category)]

  return(structure(list(dt = dt_inflation,
                        country = country,
                        category = category,
                        categories = contributions$categories,
                        level = level,
                        start_year = contributions$start_year,
                        start_month = 1,
                        end_year = contributions$end_year,
                        end_month = contributions$end_month),
                   class = "inflation"))
}

#' Calculates inflation gap between income quintiles
#'
#' @description
#' `calculate_inflation_gap()` computes the inflation gap between the lowest and highest income quintiles using the inflation data calculated by `calculate_inflation()`.
#'
#' @details
#' The function performs the following key operations:
#' 1. Identifies the lowest and highest categories from the input inflation data.
#' 2. Filters the inflation data for these two categories.
#' 3. Calculates the difference in inflation rates between these two categories.
#'
#' @param inflation An object of class `"inflation"` as produced by `calculate_inflation()`.
#'
#' @returns A `data.table` with the following columns:
#' \item{year}{year of the inflation gap.}
#' \item{month}{month of the inflation gap.}
#' \item{inflation_gap}{calculated inflation gap (lowest category - highest category).}
#'
#' @examples
#' # Calculate inflation rates for France, income category, COICOP level 2, from 2010 to 2020
#' france_inflation <- calculate_inflation("FR", "income", level = 2, start_year = 2010, end_year = 2020)
#'
#' # Calculate the inflation gap
#' inflation_gap <- calculate_inflation_gap(france_inflation)
#'
#' # Get the inflation gap for a specific year and month
#' specific_gap <- inflation_gap[year == 2015 & month == 6, inflation_gap]
#'
#' @seealso [calculate_inflation()]
#'
#' @importFrom data.table :=
#' @export
calculate_inflation_gap <- function(inflation) {
  lowest_category <- inflation$categories[1]
  highest_category <- inflation$categories[length(inflation$categories)]

  # Filter for QUINTILE1 and QUINTILE5, then dcast to wide format
  dt_wide <- data.table::dcast(inflation$dt[category %in% c(lowest_category,
                                                            highest_category)],
    year + month ~ category,
    value.var = "inflation"
  )

  # Calculate inflation gap
  dt_gap <- dt_wide[, .(
    year = year,
    month = month,
    inflation_gap = lowest_category - highest_category
  )]

  return(dt_gap)
}
