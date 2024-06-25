#' Combine CPI weights with HBS weight data
#'
#' @description
#' `calculate_weights()` merges annual Consumer Price Index (CPI) weight data with Household Budget Survey (HBS) weight data to create a comprehensive dataset for economic analysis. The merge is performed based on COICOP (Classification of Individual Consumption According to Purpose) codes and the most recent HBS wave available for each CPI data point.
#'
#' Key features:
#' 1. Temporal matching: CPI weights are matched with the most recent prior HBS wave. If no prior wave exists, the earliest available HBS wave is used.
#' 2. COICOP code alignment: The function performs a left join on COICOP codes, ensuring all CPI COICOP codes are represented in the final dataset.
#' 3. Weight normalization: Final weights are normalized to sum to 100% for each category and year.
#'
#' @details
#' Merging logic:
#' * For each CPI weight year, the function selects HBS data from the most recent prior wave.
#' * Example: CPI weights from 2015-2019 are merged with the 2015 HBS wave; 2020 CPI weights with the 2020 HBS wave.
#' * For countries with limited HBS data (e.g., France's earliest wave is 2005), all earlier CPI data points use the earliest available HBS wave.
#'
#' Data handling:
#' * COICOP codes present in CPI data but absent in HBS data are included with minimal consumption values (1e-6) to avoid data loss.
#' * Zero values in both CPI and HBS data are replaced with 1e-6 to prevent division by zero errors.
#'
#' Weight calculation:
#' The function calculates final weights using the following steps:
#' 1. Multiply CPI weights with HBS consumption data.
#' 2. Normalize by dividing by total consumption for each COICOP code and year.
#' 3. Scale weights to sum to 100% for each category and year.
#'
#' @param country A character string. ISO 3166-1 alpha-2 (2-digit) country code.
#' @param category A character string. HBS data category: "income", "age", or "urban".
#' @param level An integer. COICOP level (1-3). Default is 2.
#' @param start_year An integer. Start year for the analysis period. Default is NULL (earliest available data).
#' @param end_year An integer. End year for the analysis period. Default is NULL (latest available data).
#'
#' @return A data.table with the following columns:
#' * series_name (character): Identifier for the data series
#' * coicop (character): COICOP code
#' * year (numeric): Year of the HBS data
#' * category (character): HBS category (e.g., "QUINTILE1" for income quintile 1)
#' * weighted_consumption (numeric): Calculated weight (normalized to sum to 100 within each category and year)
#' * weight_year (numeric): Year of the CPI weight data
#'
#' @examples
#' # Calculate weights for France, income category, COICOP level 2, from 2010 to 2020
#' france_weights <- calculate_weights("FR", "income", level = 2, start_year = 2010, end_year = 2020)
#'
#' # Check if weights sum to 100 for a specific category and year
#' france_weights[category == "QUINTILE1" & weight_year == 2015, sum(weighted_consumption)]
#'
#' @seealso [load_index_weights()], [load_hbs()]
#'
#' @importFrom data.table :=
#' @export
calculate_weights <- function(country, category, level = 2,
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

  # Download data
  dt_weights <- load_index_weights(country,
    level = level,
    start_year = start_year, end_year = end_year
  )
  dt_hbs <- load_hbs(country, category,
    level = level,
    start_year = start_year, end_year = end_year
  )

  # Select COICOP codes
  hbs_coicops <- unique(dt_hbs$coicop)
  weight_coicops <- unique(dt_weights$coicop)

  # We do not use COICOP codes that have HBS data but not CPI data
  dt_hbs <- dt_hbs[coicop %in% weight_coicops, ]

  # Replace 0 with very small values to avoid division by zero (vectorized)
  dt_hbs[, consumption := pmax(consumption, 1e-6)]
  dt_weights[, weight := pmax(weight, 1e-6)]

  # Necessary before the join
  data.table::setnames(dt_weights, "year", "weight_year")

  # COICOP codes that have CPI data but not HBS data
  missing_coicops <- setdiff(weight_coicops, hbs_coicops)

  # Create new rows for missing coicops
  if (length(missing_coicops) > 0) {
    new_rows <- data.table::CJ(
      coicop = missing_coicops,
      year = unique(dt_hbs$year),
      category = unique(dt_hbs$category)
    )
    new_rows[, `:=`(
      consumption = 1e-6,
      series_name = NA_character_
    )]
    dt_hbs <- data.table::rbindlist(list(dt_hbs, new_rows), use.names = TRUE, fill = TRUE)
  }

  # Set keys for faster joining
  data.table::setkey(dt_hbs, coicop, year)
  data.table::setkey(dt_weights, coicop, weight_year)

  # Now perform the cartesian product (left join)
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

  # Remove intermediate columns to reduce memory usage
  dt_weighted_consumption[, `:=`(
    consumption = NULL,
    preweighted_consumption = NULL,
    total_consumption = NULL,
    unnormalized_weighted_consumption = NULL
  )]

  return(dt_weighted_consumption)
}
