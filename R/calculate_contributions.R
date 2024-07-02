.onLoad <- function(libname, pkgname) {
  load_cpi <<- memoise::memoise(load_cpi)
  calculate_weights <<- memoise::memoise(calculate_weights)
}

#' Calculates inflation contributions
#'
#' @description
#' `calculate_contributions()` computes the contributions to inflation for different COICOP (Classification of Individual Consumption According to Purpose) categories over time, using Consumer Price Index (CPI) data and weighted consumption data.
#'
#' @details
#' The function performs the following key operations:
#' 1. Loads CPI data and calculates weights using `load_cpi()` and `calculate_weights()`.
#' 2. Filters data to ensure consistency across COICOP codes and years.
#' 3. Calculates contributions using a formula that accounts for year-over-year changes in prices and weights.
#' 4. Handles multiple categories (e.g., income groups, age groups) simultaneously.
#'
#' The contribution calculation is based on the following formula:
#' Contribution = (P_{y-1,12} / P_{y-1,m}) * w_{y-1,j,q} * ((P_{y,m,j} - P_{y-1,12,j}) / P_{y-1,12,j}) +
#'                (P_{y-2,12} / P_{y-1,m}) * w_{y-2,j,q} * ((P_{y-1,12,j} - P_{y-1,m,j}) / P_{y-2,12,j})
#'
#' Where:
#' * P: Price index
#' * w: Weight
#' * y: Year
#' * m: Month
#' * j: COICOP category
#' * q: Demographic category (e.g., income quintile)
#'
#' @param country 2-digit country code (see ISO 3166-1 alpha-2), only one country at a time is accepted.
#' @param category Category for which to calculate contributions: "income", "age", or "urban".
#' @param level COICOP level. Possible values are 1-3. For example, "01" is level 1 and "012" is level 2. Default value is 2.
#' @param start_year year of start date.
#' @param start_month month of start date.
#' @param end_year year of end date.
#' @param end_month month of end date.
#'
#' @returns An object of class `"contributions"` is a list containing the following components:
#' \item{dt}{a `data.table` object (see below).}
#' \item{country}{2-digit country code (see ISO 3166-1 alpha-2).}
#' \item{category}{Category for which contributions were calculated: "income", "age", or "urban".}
#' \item{categories}{(Ordered) vector of category types, from lowest to highest.}
#' \item{level}{COICOP level.}
#' \item{start_year}{first year of data.}
#' \item{start_month}{first month of data.}
#' \item{end_year}{last year of data.}
#' \item{end_month}{last month of data.}
#'
#' The component `dt` has the following columns:
#' \item{year}{year of the contribution.}
#' \item{coicop}{COICOP code.}
#' \item{month}{month of the contribution.}
#' \item{category}{demographic category (e.g., income group, age group).}
#' \item{contribution}{calculated contribution to inflation.}
#'
#' @examples
#' # Calculate inflation contributions for France, income category, COICOP level 2, from 2010 to 2020
#' france_contributions <- calculate_contributions("FR", "income", level = 2, start_year = 2010, end_year = 2020)
#'
#' # Access the data.table component
#' dt_contributions <- france_contributions$dt
#'
#' # Sum contributions for a specific year, month, and category
#' total_contribution <- dt_contributions[year == 2015 & month == 6 & category == "First quintile", sum(contribution)]
#'
#' @seealso [load_cpi()], [calculate_weights()]
#'
#' @importFrom data.table :=
#' @export
calculate_contributions <- function(country, category, level = 2,
                                    start_year = NULL, start_month = NULL,
                                    end_year = NULL, end_month = NULL) {
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

  # Set start year 2 years behind due to the requirements of the equation
  start_year <- if (!is.null(start_year)) {
    start_year - 2
  } else {
    start_year
  }

  # Load data
  cpi <- load_cpi(country,
    level = level,
    start_year = start_year, start_month = start_month,
    end_year = end_year, end_month = end_month
  )
  weights <-
    calculate_weights(country, category,
      level = level,
      start_year = start_year, end_year = end_year
    )

  # Select COICOP codes
  cpi_coicops <- unique(cpi$dt$coicop)
  weight_coicops <- unique(weights$dt$coicop)

  # We do not use COICOP codes that have HBS data but not CPI data
  weights$dt <- weights$dt[coicop %in% cpi_coicops, ]
  rejected_coicops <- setdiff(weight_coicops, cpi_coicops)
  if (length(rejected_coicops) > 0) {
    message(sprintf("The following COICOP codes, found in HBS data, are removed for not being included in CPI data: %s", paste(rejected_coicops, collapse = ", ")))
  }

  # Create a sequence of all valid year-month combinations
  all_dates <- data.table::data.table(
    year = rep((cpi$start_year):(cpi$end_year), each = 12),
    month = rep(1:12, times = cpi$end_year - cpi$start_year + 1)
  )
  all_dates <- all_dates[year < cpi$end_year | (year == cpi$end_year & month <= cpi$end_month)]

  # Create a complete grid of all combinations
  cpi_complete_grid <- all_dates[, .(coicop = cpi_coicops), by = .(year, month)]
  weight_complete_grid <- data.table::CJ(coicop = cpi_coicops,
                                         category = weights$categories,
                                         weight_year = all_dates$year,
                                         unique = TRUE)

  # Merge the complete grid with the original data
  cpi_result <- merge(cpi_complete_grid, cpi$dt, by = c("coicop", "year", "month"), all.x = TRUE)
  weights_result <- merge(weight_complete_grid, weights$dt, by = c("coicop", "category", "weight_year"), all.x = TRUE)

  # Fill in missing values
  cpi_result[is.na(series_name), `:=`(series_name = NA_character_, value = 1e-6)]
  weights_result[is.na(series_name), `:=`(series_name = NA_character_,
                                          weighted_consumption = 1e-6,
                                          year = NA)]

  # If you want to keep only the columns from the original data.table
  cpi$dt <- cpi_result[, .(series_name, coicop, value, year, month)]
  weights$dt <- weights_result[, .(series_name, coicop, category, weight_year, year, weighted_consumption)]

  # We also have to assume that for a given COICOP code, the index weight years in the weighted consumption table are exactly the same as the index value years in the CPI table!
  # Hence,
  for (coicop_code in cpi_coicops) {
    if (!identical(unique(cpi$dt[coicop == coicop_code, year]), unique(weights$dt[coicop == coicop_code, weight_year]))) {
      message(coicop_code)
      stop("We got a problem!")
    }
  }

  # COICOP codes that have CPI data but not weight data
  missing_coicops <- setdiff(cpi_coicops, weight_coicops)
  if (length(missing_coicops) > 0) {
    stop("This should not be possible!")
  }

  contrib2 <- data.table::data.table(
    year = numeric(),
    coicop = character(),
    month = numeric(),
    category = character(),
    contribution = numeric()
  )

  pb <- progress::progress_bar$new(
    format = "calculating contributions [:bar] :percent eta: :eta (elapsed: :elapsed)",
    total = length((cpi$start_year):(cpi$end_year)) - 2, clear = FALSE
  )
  pb$tick(0)

  categories <- unique(weights$dt$category)

  for (y in (cpi$start_year+2):(cpi$end_year)) {
    # This can be further optimised since each COICOP code is independent
    for (j in cpi_coicops) {
      # Constant values
      p_y1_12 <- sum(cpi$dt[month == 12 &
        year == y - 1, value])
      p_y2_12 <- sum(cpi$dt[month == 12 &
        year == y - 2, value])
      p_y1_12_j <- cpi$dt[coicop == j &
        month == 12 &
        year == y - 1, value]
      p_y2_12_j <- cpi$dt[coicop == j &
        month == 12 &
        year == y - 2, value]

      # Calculate HBS weights across all categories
      w_y1_j_q <- weights$dt[coicop == j &
        weight_year == y - 1, .(category, weighted_consumption)]
      data.table::setkey(w_y1_j_q, category)
      w_y2_j_q <- weights$dt[coicop == j &
        weight_year == y - 2, .(category, weighted_consumption)]

      # Calculate index weights across all months
      # (not necessarily 12 for the latest year)
      p_y1_m <- cpi$dt[year == y - 1, sum(value), by = .(month)]
      data.table::setkey(p_y1_m, month)
      p_y1_m_j <- cpi$dt[coicop == j &
        year == y - 1, .(month, value)]
      p_y_m_j <- cpi$dt[coicop == j &
        year == y, .(month, value)]

      # Cross join month and category
      dt_cj <- data.table::CJ(
        month = p_y1_m$month,
        category = w_y1_j_q$category
      )

      # Fill up columns according to their respective keys
      dt_cj[, w_y1_j_q := w_y1_j_q[.SD, on = .(category), weighted_consumption]]
      dt_cj[, w_y2_j_q := w_y2_j_q[.SD, on = .(category), weighted_consumption]]
      dt_cj[, p_y1_m := p_y1_m[.SD, on = .(month), V1]]
      dt_cj[, p_y1_m_j := p_y1_m_j[.SD, on = .(month), value]]
      dt_cj[, p_y_m_j := p_y_m_j[.SD, on = .(month), value]]

      # Apply formula
      dt_cj[, contribution := (p_y1_12 / p_y1_m) * w_y1_j_q * ((p_y_m_j - p_y1_12_j) / p_y1_12_j) +
        (p_y2_12 / p_y1_m) * w_y2_j_q * ((p_y1_12_j - p_y1_m_j) / p_y2_12_j)]

      # Clean up
      dt_cj[, `:=`(year = y, coicop = j)]
      dt_cj <- dt_cj[!is.na(contribution), .(year, coicop, month, category, contribution)]

      # Bind the new contributions to the existing data table
      contrib2 <- data.table::rbindlist(list(contrib2, dt_cj), use.names = TRUE)
    }
    pb$tick()
  }

  return(structure(list(dt = contrib2,
                        country = country,
                        category = category,
                        categories = weights$categories,
                        level = level,
                        start_year = min(contrib2$year),
                        start_month = 1,
                        end_year = max(contrib2$year),
                        end_month = max(contrib2[year == max(contrib2$year), month])),
                   class = "contributions"))
}

memo_calculate_contributions <- memoise::memoise(calculate_contributions)

# Function to check if a weight_year has all unique COICOP codes
has_all_coicop <- function(data, year, coicops) {
  coicop_in_year <- unique(data$coicop[data$year == year])
  all(coicops %in% coicop_in_year)
}
