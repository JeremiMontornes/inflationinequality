#' @importFrom data.table :=
new_hbs <- function(dt = data.table::data.table(), dt_total = data.table::data.table(),
                    country = character(), category = character(),
                    categories = vector(), level = numeric()) {
  stopifnot(data.table::is.data.table(dt))
  stopifnot(data.table::is.data.table(dt_total))
  stopifnot(is.character(country))
  stopifnot(is.character(category))
  stopifnot(is.vector(categories))
  stopifnot(is.numeric(level))

  # Remove empty keys
  dt <- dt[!is.na(coicop) | !is.na(year) | !is.na(category)]
  dt_total <- dt_total[!is.na(coicop) | !is.na(year)]

  # Ensure strictly positive consumption
  dt[, consumption := pmax(consumption, 1e-6, na.rm = TRUE)]

  # Select rows where total_consumption is NA or non-positive
  missing_total_weights <-
    dt_total[is.na(total_consumption) | total_consumption <= 0, ]

  if (nrow(missing_total_weights) > 0) {
    message(
      "There are some total consumption weights that are missing.
      These will be calculated as a simple average:\n",
      paste(capture.output(print(missing_total_weights[, .(coicop, year)])),
            collapse = "\n"))

    # Identify incoherent total_consumption values and update them
    dt_total[
      missing_total_weights,
      on = .(coicop, year),
      total_consumption := dt[
        .SD, # Join with dt
        on = .(coicop, year), # Join based on coicop and year
        mean(consumption), # Calculate mean consumption
        by = .EACHI # Do this for each row in the subset
      ]$V1 # Extract the resulting means as a vector
    ]
  }

  start_year = dt[, min(year)]
  end_year = dt[, max(year)]

  return(structure(
    list(
      dt = dt,
      dt_total = dt_total,
      country = country,
      category = category,
      categories = categories,
      level = level,
      start_year = start_year,
      end_year = end_year
    ),
    class = "hbs"
  ))
}

validate_hbs <- function(hbs) {
  ## Verify columns are correct
  required_columns <- c("series_name", "coicop", "year", "consumption", "category")
  missing_columns <- setdiff(required_columns, names(hbs$dt))

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from 'dt': ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!hbs$level %in% 1:3) {
    stop("COICOP level must be 1, 2 or 3.")
  }

  ## Verify data are coherent
  if (nrow(hbs$dt[is.na(year) | is.na(coicop) | is.na(category), ]) > 0
      | nrow(hbs$dt_total[is.na(year) | is.na(coicop), ]) > 0) {
    stop("Data are not coherent, there are some NA values")
  }
  if (!(is.character(hbs$dt[, series_name]) &
        is.character(hbs$dt[, coicop]) &
        is.character(hbs$dt[, category]) &
        is.numeric(hbs$dt[, year]) &
        is.numeric(hbs$dt[, consumption]) &
        is.character(hbs$dt_total[, coicop]) &
        is.numeric(hbs$dt_total[, year]) &
        is.numeric(hbs$dt_total[, total_consumption]))) {
    stop("Data are not coherent, data types are not correct")
  }
  if (nrow(hbs$dt[consumption <= 0, ]) > 0
      | nrow(hbs$dt_total[total_consumption <= 0,]) > 0) {
    stop("Data are not coherent, HBS weights must be strictly positive (>0)")
  }
  if (nrow(hbs$dt[nchar(coicop) > hbs$level + 1, ]) > 0) {
    stop("Data are not coherent, there are COICOP codes with the incorrect level")
  }
  if (nrow(hbs$dt[coicop == "00", ]) > 0) {
    stop("Data are not coherent, COICOP code 00 cannot exist in `dt`")
  }

  ## No duplicates
  if (anyDuplicated(hbs$dt[, .(coicop, year, category)])
      | anyDuplicated(hbs$dt_total[, .(coicop, year)])) {
    stop("Data contain duplicates")
  }

  # We may need to verify that for each (coicop, year) pair in dt, there exists
  # all categories in the attribute categories.
  # Also, we would need to check that for each (coicop, year) in dt, there exist
  # the same (coicop, year) pair in dt_total.

  hbs
}

#' Constructor for HBS data object
#'
#' @description
#' `hbs()` constructs an object of class `"hbs"` which contains HBS (Household
#' Budget Survey) data.
#'
#' @param dt a `data.table` object (see details).
#' @param dt_total a `data.table` object (see details).
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one
#'   country at a time is accepted.
#' @param category HBS category: `"income"`, `"age"`, `"urban"`.
#' @param categories (Ordered) vector of category types, from lowest to highest.
#' @param level COICOP level. Possible values are 1-3. For example, "01" is
#'   level 1 (Division), "012" is level 2 (Group), and "0111" is level 3
#'   (Class).
#'
#' @details
#' The component `dt` has the following columns:
#'
#' - `series_name`: identifier for the data series.
#' - `coicop`: COICOP code.
#' - `year`: year of data series.
#' - `category`: category type within the specified HBS category.
#' - `consumption`: consumption value for the specified category and COICOP
#' code.
#'
#' For every (COICOP, year) pair, there should be 1 row for each available
#' category.
#'
#' The component `dt_total` has the following columns:
#'
#' - `series_name`: identifier for the data series.
#' - `coicop`: COICOP code.
#' - `year`: year of data series.
#' - `total_consumption`: consumption value for the specified category and
#' COICOP code.
#'
#' For each (COICOP, year) pair in `dt`, there should be a corresponding
#' (COICOP, year) pair in `dt_total`.
#'
#' @returns An object of class `"hbs"` is a list containing the following
#' components:
#'
#' - `dt`: a `data.table` object, each row in `dt` represents the weighting
#' of a specific item category in a demographic category in a particular year
#' (see details).
#' - `dt_total`: a `data.table` object, each row in `dt` represents the
#' weighting of a specific item category for the entire population in a
#' particular year (see details).
#' - `country`: 2-digit country code (see ISO 3166-1 alpha-2).
#' - `category`: HBS category: `"income"`, `"age"`, `"urban"`.
#' - `categories`: (Ordered) vector of category types, from lowest to highest.
#' - `level`: COICOP level.
#' - `start_year`: first year of data.
#' - `end_year`: last year of data.
#'
#' @examples
#' # Create a sample HBS dataset
#' dt <- data.table(
#'   series_name = c("HBS", "HBS", "HBS", "HBS"),
#'   coicop = c("01", "02", "01", "02"),
#'   year = c(2022, 2022, 2023, 2023),
#'   category = c("A", "A", "B", "B"),
#'   consumption = c(100, 200, 150, 250)
#' )
#'
#' dt_total <- data.table(
#'   coicop = c("01", "02", "01", "02"),
#'   year = c(2022, 2022, 2023, 2023),
#'   total_consumption = c(300, 400, 350, 450)
#' )
#'
#' # Create an HBS object
#' my_hbs <- hbs(dt = dt,
#'               dt_total = dt_total,
#'               country = "FR",
#'               category = "income",
#'               categories = c("A", "B"),
#'               level = 1)
#'
#' @export
hbs <- function(dt = data.table::data.table(), dt_total = data.table::data.table(),
                country = character(), category = character(),
                categories = vector(), level = numeric()) {
  validate_hbs(new_hbs(dt, dt_total, country, category, categories, level))
}

#' Interpolate HBS data
#'
#' @description
#' This function performs linear interpolation on Household Budget Survey (HBS) data.
#' It interpolates consumption values for each year between the minimum and maximum
#' years present in the original data.
#'
#' @param hbs an object of class `"hbs"`.
#'
#' @return an object of class "hbs" with interpolated data.
#'
#' @details
#' The function uses the `approx()` function to perform linear interpolation.
#' For the main data table (`dt`), it interpolates by coicop and category.
#' For the total consumption data table (`dt_total`), it interpolates by coicop only.
#'
#' The interpolation creates a row for each year between the minimum and maximum
#' years in the original data, for each group (coicop or coicop+category).
#'
#' - The function assumes that the input `hbs` object has `dt` and `dt_total` components.
#' - In the resulting `dt`, a `series_name` column is added with NA values.
#' - In `dt_total`, the 'consumption' column is renamed to 'total_consumption' for clarity.
#'
#' @examples
#' my_hbs <- load_hbs("FR", "income", start_year = 2005)
#' interpolated_hbs <- interpolate_hbs(my_hbs)
#'
#' @export
interpolate_hbs <- function(hbs) {
  UseMethod("interpolate_hbs")
}

#' @exportS3Method
interpolate_hbs.hbs <- function(hbs) {

  # Function to interpolate for a single group
  interpolate_group <- function(years, consumptions) {
    new_years <- seq(min(years), max(years), by = 1)
    new_consumptions <- approx(years, consumptions, xout = new_years)$y
    data.table::data.table(year = new_years, consumption = new_consumptions)
  }

  # Apply the interpolation
  dt <- hbs$dt[, interpolate_group(year, consumption), by = .(coicop, category)] %>%
    .[, .(series_name = NA_character_, coicop, year, category, consumption)]
  hbs$dt_total[, consumption := total_consumption]
  dt_total <-
    hbs$dt_total[, interpolate_group(year, total_consumption), by = .(coicop)] %>%
    .[, .(coicop, year, total_consumption = consumption)]

  hbs(dt = dt, dt_total = dt_total,
      country = hbs$country,
      category = hbs$category,
      categories = hbs$categories,
      level = hbs$level)
}

#' Add missing COICOPS
#'
#' @description
#' This function adds missing COICOP (Classification of Individual Consumption According to Purpose) codes to a household budget survey (HBS) dataset.
#'
#' @param hbs An object of class `"hbs"` containing household budget survey data.
#' @param coicops A vector of COICOP codes to be added if missing.
#'
#' @return An updated `"hbs"` object with new rows for missing COICOP codes.
#'
#' @examples
#' \dontrun{
#' # Add missing COICOPs
#' updated_hbs <- add_coicops_hbs(my_hbs, c("03", "04"))
#' }
#'
#' @export
add_coicops_hbs <- function(hbs, coicops) {
  UseMethod("add_coicops_hbs")
}

#' @exportS3Method
add_coicops_hbs.hbs <- function(hbs, coicops) {
  existing_coicops <- intersect(coicops, hbs$dt[, unique(coicop)])
  if (length(existing_coicops) > 0) {
    warning(
      sprintf("The following COICOP codes, found in HBS data, already exist: %s", paste(existing_coicops, collapse = ", "))
    )
  }
  coicops_to_be_added <- setdiff(coicops, existing_coicops)

  # Create new rows for missing coicops
  new_rows <- data.table::CJ(
    coicop = coicops_to_be_added,
    year = hbs$start_year:hbs$end_year,
    category = hbs$categories
  )

  # Set default values for new rows
  new_rows[, `:=`(
    consumption = 1e-6,
    series_name = NA_character_
  )]

  # Combine existing data with new rows
  dt <- data.table::rbindlist(list(hbs$dt, new_rows), use.names = TRUE, fill = TRUE)

  # Create new rows for missing coicops
  new_rows_total <- data.table::CJ(
    coicop = coicops_to_be_added,
    year = hbs$start_year:hbs$end_year
  )

  # Set default values for new rows
  new_rows_total[, `:=`(
    total_consumption = 1e-6
  )]

  dt_total <- data.table::rbindlist(list(hbs$dt_total, new_rows_total), use.names = TRUE, fill = TRUE)

  hbs(dt = dt,
      dt_total = dt_total,
      country = hbs$country,
      category = hbs$category,
      categories = hbs$categories,
      level = hbs$level)
}
