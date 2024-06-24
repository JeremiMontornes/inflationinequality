#' Calculate inflation rates
#'
#' @description
#' `calculate_inflation()` computes inflation rates for different categories over time. It uses the contributions calculated by `calculate_contributions()` to sum up the total inflation for each category, year, and month.
#'
#' The function returns a `data.table` with the following columns:
#' * `year` (`num`): The year of the inflation rate
#' * `month` (`num`): The month of the inflation rate
#' * `category` (`chr`): The category (e.g., income group, age group, urban/rural)
#' * `inflation` (`num`): The calculated inflation rate
#'
#' @param country ISO 3166-1 alpha-2 (2 digit) country code.
#' @param category Category for which to calculate inflation: "income", "age", "urban"
#' @param level COICOP level. Default value is 2. Possible values are 1-3.
#' @param start_year Year of start date. Default value is NULL.
#' @param start_month Month of start date. Default value is NULL.
#' @param end_year Year of end date. Default value is NULL.
#' @param end_month Month of end date. Default value is NULL.
#' @returns A `data.table` object containing the calculated inflation rates.
#' @seealso [calculate_contributions()]
#' @importFrom data.table :=
#' @export
calculate_inflation <- function(country, category, level = 2,
                                start_year = NULL, start_month = NULL,
                                end_year = NULL, end_month = NULL) {
  dt_contributions <- calculate_contributions(country, category,
    level = level,
    start_year = start_year, start_month = start_month,
    end_year = end_year, end_month = end_month
  )
  dt_inflation <- dt_contributions[, .(inflation = sum(contribution)), by = .(year, month, category)]
}

#' Calculate inflation gap between income quintiles
#'
#' @description
#' `calculate_inflation_gap()` computes the inflation gap between the lowest (QUINTILE1) and highest (QUINTILE5) income quintiles. It takes the inflation data calculated by `calculate_inflation()` and returns the difference in inflation rates between these two quintiles.
#'
#' The function returns a `data.table` with the following columns:
#' * `year` (`num`): The year of the inflation gap
#' * `month` (`num`): The month of the inflation gap
#' * `inflation_gap` (`num`): The calculated inflation gap (QUINTILE1 - QUINTILE5)
#'
#' @param dt_inflation A `data.table` containing inflation data, as produced by `calculate_inflation()`.
#'        Must include columns for year, month, category, and inflation.
#' @returns A `data.table` object containing the calculated inflation gap.
#' @note This function only works with inflation data categorized by income quintiles.
#'       It specifically uses QUINTILE1 and QUINTILE5 for the calculation.
#' @seealso [calculate_inflation()]
#' @importFrom data.table :=
#' @export
calculate_inflation_gap <- function(dt_inflation) {
  # Ensure dt_inflation is a data.table
  data.table::setDT(dt_inflation)

  # Filter for QUINTILE1 and QUINTILE5, then dcast to wide format
  dt_wide <- data.table::dcast(dt_inflation[category %in% c("QUINTILE1", "QUINTILE5")],
    year + month ~ category,
    value.var = "inflation"
  )

  # Calculate inflation gap
  dt_gap <- dt_wide[, .(
    year = year,
    month = month,
    inflation_gap = QUINTILE1 - QUINTILE5
  )]

  return(dt_gap)
}
