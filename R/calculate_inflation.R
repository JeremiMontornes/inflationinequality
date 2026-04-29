#' Calculates inflation rates
#'
#' @description
#' `calculate_inflation()` computes inflation rates for different categories
#' over time using the contributions calculated by `calculate_contributions()`.
#'
#' @details
#' The function performs the following key operations:
#' 1. Calls `calculate_contributions()` to get the inflation contributions
#' 2. Sums up the total inflation for each category, year, and month.
#'
#' @inheritParams calculate_contributions
#'
#' @returns An object of class `"inflation"` is a list containing the following
#'   components:
#'
#' - `dt`: a `data.table` object (see below).
#' - `dt_missing_weights`: a `data.table` object (see below).
#' - `dt_coverage`: a `data.table` object (see below).
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `category`: Category for which contributions were calculated: "income",
#' "age", or "urban".
#' - `categories`: (Ordered) vector of category types, from lowest to highest.
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `start_month`: first month of data.
#' - `end_year`: last year of data.
#' - `end_month`: last month of data.
#'
#' The component `dt` has the following columns:
#'
#' - `year`: year of the inflation rate.
#' - `month`: month of the inflation rate.
#' - `category`: demographic category (e.g., income group, age group).
#' - `contribution`: calculated inflation rate.
#'
#' The component `dt_missing_weights` has the following columns:
#' \describe{
#'   \item{coicop}{COICOP code}
#'   \item{year}{year}
#'   \item{category}{demographic category}
#'   \item{missing_weight}{weight of product category}
#' }
#'
#' The component `dt_coverage` has the following columns:
#' \describe{
#'   \item{weight_year}{year}
#'   \item{weight_sum_avg}{total weight coverage of price index in percentage points}
#' }
#'
#' @examples
#' \dontrun{
#' # Calculate inflation rates for France, income category  from 2010 to 2020
#' france_inflation <- calculate_inflation("FR", "income", start_year = 2010, end_year = 2020)
#'
#' # Access the data.table component
#' dt_inflation <- france_inflation$dt
#'
#' # Get inflation rate for a specific year, month, and category
#' specific_inflation <- dt_inflation[year == 2015 & month == 6 & category == "First quintile", inflation]
#' }
#'
#' @seealso [calculate_contributions()]
#'
#' @importFrom data.table :=
#' @export
calculate_inflation <- function(country = NULL, category = NULL, level = 2,
                                start_year = NULL, start_month = NULL,
                                end_year = NULL, end_month = NULL,
                                ensure_complete_cpi = FALSE,
                                custom_cpi = NULL,
                                custom_index_weights = NULL,
                                custom_hbs = NULL,
                                interpolated_hbs = FALSE,
                                specific_hbs_year = NULL) {
  contributions <- calculate_contributions(country, category,
    level = level,
    start_year = start_year, start_month = start_month,
    end_year = end_year, end_month = end_month,
    ensure_complete_cpi = ensure_complete_cpi,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    interpolated_hbs = interpolated_hbs,
    specific_hbs_year = specific_hbs_year
  )
  dt_inflation <-
    contributions$dt[, .(inflation = sum(contribution)),
                     by = .(year, month, category)]

  return(structure(list(dt = dt_inflation,
                        dt_missing_weight = contributions$dt_missing_weights,
                        dt_coverage = contributions$dt_coverage,
                        country = country,
                        category = category,
                        categories = contributions$categories,
                        start_year = contributions$start_year,
                        start_month = 1,
                        end_year = contributions$end_year,
                        end_month = contributions$end_month),
                   class = "inflation"))
}

#' Calculates inflation gap between income quintiles
#'
#' @description
#' `calculate_inflation_gap()` computes the inflation gap between the lowest and
#' highest income quintiles using the inflation data calculated by
#' `calculate_inflation()`.
#'
#' @details
#' The function performs the following key operations:
#' 1. Identifies the lowest and highest categories from the input inflation data.
#' 2. Filters the inflation data for these two categories.
#' 3. Calculates the difference in inflation rates between these two categories.
#'
#' @param inflation An object of class `"inflation"`.
#'
#' @returns A `data.table` with the following columns:
#'
#' - `year`: year of the inflation gap.
#' - `month`: month of the inflation gap.
#' - `inflation_gap`: calculated inflation gap (lowest category - highest
#' category).
#'
#' @examples
#' # Calculate inflation rates for France, income category from 2010 to 2020
#' france_inflation <- calculate_inflation("FR", "income", start_year = 2010, end_year = 2020)
#'
#' # Calculate the inflation gap
#' inflation_gap <- calculate_inflation_gap(france_inflation)
#'
#' # Get the inflation gap for a specific year and month
#' specific_gap <- inflation_gap[year == 2015 & month == 6, inflation_gap]
#'
#' @seealso [calculate_inflation()]
#'
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
    inflation_gap = get(lowest_category) - get(highest_category)
  )]

  return(dt_gap)
}

#' Calculate average monthly inflation
#'
#' @param inflation An object of class `"inflation"`.
#'
#' @returns A `data.table` with the following columns:
#'
#' - `year`: year of the inflation gap.
#' - `month`: month of the inflation gap.
#' - `total_inflation`: average inflation rate.
#'
#' @examples
#' # Calculate inflation rates for France, income category from 2010 to 2020
#' france_inflation <- calculate_inflation("FR", "income", start_year = 2010, end_year = 2020)
#'
#' # Calculate average monthly inflation
#' dt_monthly_inflation_fr <- calculate_total_inflation(france_inflation)
#'
#' @seealso [calculate_inflation()]
#'
#' @export
calculate_total_inflation <- function(inflation) {
  inflation$dt[, .(total_inflation = mean(inflation)), by = .(year, month)]
}
