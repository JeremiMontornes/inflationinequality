#' Calculates combined weights from CPI and HBS data
#'
#' @description
#' `calculate_weights()` combines annual Consumer Price Index (CPI) weight data with Household Budget Survey (HBS) weight data to create a comprehensive dataset for economic analysis.
#'
#' @details
#' The function performs the following key operations:
#' 1. Temporal matching: CPI weights are matched with the most recent prior HBS wave. If no prior wave exists, the earliest available HBS wave is used.
#' 2. COICOP code alignment: A left join is performed on COICOP codes, ensuring all CPI COICOP codes are represented in the final dataset.
#' 3. Weight normalization: Final weights are normalized to sum to 100% for each category and year.
#'
#' Merging logic:
#' * For each CPI weight year, the function selects HBS data from the most recent prior wave.
#' * Example: CPI weights from 2015-2019 are merged with the 2015 HBS wave; 2020 CPI weights with the 2020 HBS wave.
#' * For countries with limited HBS data (e.g., France's earliest wave is 2005), all earlier CPI data points use the earliest available HBS wave.
#'
#' Data handling:
#' * COICOP codes present in CPI data but absent in HBS data are included with minimal consumption values (1e-6) to avoid data loss.
#' * Zero values in both CPI and HBS data are replaced with 1e-6 to prevent division by zero errors.
#'
#' Weight calculation steps:
#' 1. Multiply CPI weights with HBS consumption data.
#' 2. Normalize by dividing by total consumption for each COICOP code and year.
#' 3. Scale weights to sum to 100% for each category and year.
#'
#' @inheritParams load_hbs
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one
#'   country at a time is accepted. The parameter is optional since we can load
#'   `custom_index_weights` and `custom_hbs`, but if both of those arguments are
#'   missing, then the `country` parameter is mandatory.
#' @param category HBS data by category: `"income"`, `"age"`, `"urban"`. The
#'   parameter is optional since one can load `custom_hbs`, but it is mandatory
#'   if that argument is missing.
#' @param custom_index_weights an object of class `"index_weights"`.
#' @param custom_hbs an object of class `"hbs"`.
#' @param interpolated_hbs flag if you want to interpolate HBS weights
#' @param specific_hbs_year year of selected HBS wave. It's recommended to
#'   download HBS data first to see what HBS years are available.
#'
#' @returns An object of class `"weights"` is a list containing the following
#'   components:
#' - `dt`: a `data.table` object (see below).
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `category`: HBS category: `"income"`, `"age"`, or `"urban"`.
#' - `categories`: (Ordered) vector of category types, from lowest to highest.
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `last_year`: last year of data.
#'
#' The component `dt` has the following columns:
#' - `series_name`: identifier for the data series.
#' - `coicop`: COICOP code.
#' - `year`: year of the HBS data.
#' - `category`: HBS category (e.g., "First quintile").
#' - `weighted_consumption`: calculated weight (normalized to sum to 100 within
#' each category and year).
#' - `weight_year`: year of the CPI weight data.
#'
#' @examples
#' # Calculate weights for France, income category, COICOP level 2, from 2010 to
#' 2020
#' france_weights <- calculate_weights("FR", "income", level = 2,
#' start_year = 2010, end_year = 2020)
#'
#' # Check if weights sum to 100 for a specific category and year
#' france_weights$dt[category == "First quintile" & weight_year == 2015,
#' sum(weighted_consumption)]
#'
#' # Access the data.table component
#' dt_weights <- france_weights$dt
#'
#' @seealso [load_index_weights()], [load_hbs()], [index_weights()], [hbs()]
#'
#' @importFrom data.table :=
#' @export
calculate_weights <- function(country = NULL, category = NULL, level = 2,
                              start_year = NULL, end_year = NULL,
                              custom_index_weights = NULL,
                              custom_hbs = NULL,
                              interpolated_hbs = FALSE,
                              specific_hbs_year = NULL) {
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

  # Load index weights
  index_weights <- if (is.null(custom_index_weights)) {
    if (is.null(country)) {
      stop("Either 'country' or 'custom_index_weights' must be provided.")
    }
    load_index_weights(
      country, level = level,
      start_year = start_year, end_year = end_year)
  } else {
    custom_index_weights
  }

  # Load HBS data
  hbs <- if (is.null(custom_hbs)) {
    if (is.null(country) || is.null(category)) {
      stop("Either both 'country' and 'category', or 'custom_hbs' must be provided.")
    }
    load_hbs(
      country, category,
      level = level)
  } else {
    custom_hbs
  }

  if (interpolated_hbs) {
    hbs <- interpolate_hbs(hbs)
  }

  # Select COICOP codes
  hbs_coicops <- unique(hbs$dt$coicop)
  weight_coicops <- unique(index_weights$dt$coicop)

  # We do not use COICOP codes that have HBS data but not CPI data
  dt_hbs <- hbs$dt[coicop %in% weight_coicops, ]
  rejected_coicops <- setdiff(hbs_coicops, weight_coicops)
  if (length(rejected_coicops) > 0) {
    message(sprintf("The following COICOP codes, found in HBS data, are removed for not being included in CPI data: %s", paste(rejected_coicops, collapse = ", ")))
  }

  # Necessary before the join
  if ("year" %in% names(index_weights$dt)) {
    data.table::setnames(index_weights$dt, "year", "weight_year")
  } else if (!"weight_year" %in% names(index_weights$dt)) {
    stop("Something's wrong!")
  }

  # COICOP codes that have CPI data but not HBS data
  missing_coicops <- setdiff(weight_coicops, hbs_coicops)

  # Create new rows for missing COICOP codes
  if (length(missing_coicops) > 0) {
    hbs <- add_coicops_hbs(hbs, missing_coicops)
  }

  # Include total consumption column
  dt_hbs <- dt_hbs[hbs$dt_total, on = .(coicop, year)]

  # Select specific HBS year if applicable
  if (!is.null(specific_hbs_year)) {
    dt_hbs <- dt_hbs[year == specific_hbs_year, ]
  }

  # Now perform the Cartesian product (left join)
  dt_weighted_consumption <-
    dt_hbs[index_weights$dt, on = .(coicop), allow.cartesian = TRUE]

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
  # dt_weighted_consumption[, total_consumption := sum(consumption), by = .(coicop, weight_year)]

  # Divide the 'weighted_consumption' column by the total consumption
  dt_weighted_consumption[, unnormalized_weighted_consumption := preweighted_consumption / total_consumption]
  ###

  # Normalised weights
  dt_weighted_consumption[, weighted_consumption := unnormalized_weighted_consumption * 100 / sum(unnormalized_weighted_consumption), by = .(weight_year, category)]

  # Remove intermediate columns to reduce memory usage
  dt_weighted_consumption[, `:=`(
    weight = NULL,
    consumption = NULL,
    preweighted_consumption = NULL,
    total_consumption = NULL,
    unnormalized_weighted_consumption = NULL
  )]

  return(structure(list(dt = dt_weighted_consumption,
                        country = country,
                        category = category,
                        categories = hbs$categories,
                        level = level,
                        start_year = min(dt_weighted_consumption$weight_year),
                        end_year = max(dt_weighted_consumption$weight_year)),
                   class = "weights"))
}
