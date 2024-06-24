#' Merge CPI weight and HBS consumption data
#'
#' @description
#' `calculate_weights()` merges (annual) CPI weight data with HBS consumption data by the most recent HBS wave and by COICOP code. If there is no HBS wave earlier than a CPI weight datapoint than we merge with the earliest possible HBS wave. For example, CPI weight data from 2015 to 2019 will be merged with the 2015 HBS wave, CPI weight data from 2020 will be mergerd with the 2020 HBS wave. The earliest French HBS wave is from 2005, therefore all French CPI weight data upto 2009 will be merged with the 2005 French HBS wave.
#'
#' The data is formatted as `data.table` with following columns:
#' * `series_name` (`chr`)
#' * `coicop` (`chr`)
#' * `year` (`num`)
#' * `category` (`chr`)
#' * `weighted_consumption` (`num`)
#'
#' @param country ISO 3166-1 alpha-2 (2 digit) country code.
#' @param category HBS data by category: `"income"`, `"age"`, `"urban"`
#' @param level COICOP level. Default value is 2. Possible values are 1-3.
#' @param start_year Year of start date. Default value is NULL.
#' @param end_year Year of end date. Default value is NULL.
#' @returns A `data.table` object.
#' @seealso [load_index_weights(), load_hbs()]
#' @importFrom data.table :=
#' @export
calculate_weights <- function(country, category, level = 2,
                                           start_year = NULL, end_year = NULL) {
  # Download data
  dt_weights <- load_index_weights(country,
    level = level,
    start_year = start_year, end_year = end_year
  )
  dt_hbs <- load_hbs(country, category,
    level = level,
    start_year = start_year, end_year = end_year
  )

  # Replace 0 with very small values to avoid division by zero
  dt_hbs <- dt_hbs[consumption == 0, consumption := 1e-6]
  dt_weights <- dt_weights[weight == 0, weight := 1e-6]

  # Select intersecting COICOP codes
  weight_coicops <- unique(dt_weights$coicop)
  hbs_coicops <- unique(dt_hbs$coicop)
  coicops <- intersect(weight_coicops, hbs_coicops)
  dt_hbs <- dt_hbs[coicop %in% coicops, ]
  dt_weights <- dt_weights[coicop %in% coicops, ]

  data.table::setnames(dt_weights, "year", "weight_year")
  dt_weighted_consumption <- dt_hbs[dt_weights, on = .(coicop), allow.cartesian = TRUE]

  dt_weighted_consumption <- dt_weighted_consumption[,
    {
      # Create a temporary copy of the current subset (.SD) of the data.table
      temp <- .SD

      # Check if the current group has any rows (.N > 0)
      # and if there are any rows where 'year' is less than or equal to 'weight_year'
      if (.N > 0 && any(year <= weight_year)) {
        # If both conditions are true:
        # 1. Filter the temporary data.table to keep only rows where 'year' <= 'weight_year'
        temp <- temp[year <= weight_year]

        # 2. From the filtered data.table, select the row with the maximum 'year' value
        # This is done using '.SD[which.max(year)]', which returns a data.table with a single row
        # containing the row with the maximum 'year' value in the current group
        temp <- temp[, .SD[which.max(year)]]
      } else {
        # If either condition is false (the group is empty or no rows have 'year' <= 'weight_year'):
        # Select the row with the minimum 'year' value from the temporary data.table
        # This is done using '.SD[which.min(year)]', which returns a data.table with a single row
        # containing the row with the minimum 'year' value in the current group
        temp <- temp[, .SD[which.min(year)]]
      }

      # Return the temporary data.table 'temp' after performing the desired operations
      temp

      # The 'by' argument specifies the columns to group the data by
      # In this case, the grouping is done by 'coicop', 'category', and 'weight_year'
      # This means that the operations inside the curly braces are performed for each unique
      # combination of 'coicop', 'category', and 'weight_year'
    },
    by = .(coicop, category, weight_year)
  ]

  ### Equation (1)
  # Calculate the weighted consumption by multiplying 'weight' and 'consumption' column-wise
  dt_weighted_consumption[, preweighted_consumption := weight * consumption]

  # Calculate the total consumption for the current group ('coicop', 'weight_year')
  # by summing the 'consumption' column and summing over all 'category' values
  dt_weighted_consumption[, total_consumption := sum(consumption), by = .(coicop, weight_year)]

  # Divide the 'weighted_consumption' column by the total consumption
  dt_weighted_consumption[, unnormalized_weighted_consumption := preweighted_consumption / total_consumption]
  ###

  # Normalised weights
  dt_weighted_consumption[, weighted_consumption := unnormalized_weighted_consumption * 100 / sum(unnormalized_weighted_consumption), by = .(weight_year, category)]

  return(dt_weighted_consumption)
}
