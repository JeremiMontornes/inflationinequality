#' Calculate inflation contributions
#'
#' @description
#' `calculate_contributions()` computes the contributions to inflation for different COICOP categories over time. It uses CPI (Consumer Price Index) data and weighted consumption data to calculate these contributions. The function handles data for multiple years, accounting for changes in weights and price indices.
#'
#' The function returns a `data.table` with the following columns:
#' * `year` (`num`): The year of the contribution
#' * `coicop` (`chr`): The COICOP code
#' * `month` (`num`): The month of the contribution
#' * `category` (`chr`): The category (e.g., income group, age group, urban/rural)
#' * `contribution` (`num`): The calculated contribution to inflation
#'
#' @param country ISO 3166-1 alpha-2 (2 digit) country code.
#' @param category Category for which to calculate contributions: "income", "age", "urban"
#' @param level COICOP level. Default value is 2. Possible values are 1-3.
#' @param start_year Year of start date. Default value is NULL.
#' @param start_month Month of start date. Default value is NULL.
#' @param end_year Year of end date. Default value is NULL.
#' @param end_month Month of end date. Default value is NULL.
#' @returns A `data.table` object containing the calculated contributions.
#' @seealso [load_cpi()], [calculate_weights()]
#' @importFrom data.table :=
#' @export
calculate_contributions <- function(country, category, level = 2,
                                    start_year = NULL, start_month = NULL,
                                    end_year = NULL, end_month = NULL) {
  start_year <- if (!is.null(start_year)) {
    start_year - 2
  } else {
    start_year
  }

  # Load data
  dt_cpi <- load_cpi(country,
    level = level,
    start_year = start_year, start_month = start_month,
    end_year = end_year, end_month = end_month
  )
  dt_weights <-
    calculate_weights(country, category,
      level = level,
      start_year = start_year, end_year = end_year
    )

  # Select COICOP codes
  cpi_coicops <- unique(dt_cpi$coicop)
  weight_coicops <- unique(dt_weights$coicop)

  # We do not use COICOP codes that have HBS data but not CPI data
  dt_weights <- dt_weights[coicop %in% cpi_coicops, ]

  # Select years with all COICOP codes present
  years_with_all_coicops <- unique(dt_cpi$year)[sapply(unique(dt_cpi$year), function(yr) has_all_coicop(dt_cpi, yr, cpi_coicops))]

  # Filter data accordingly
  dt_cpi <- dt_cpi[year %in% years_with_all_coicops, ]
  dt_weights <-
    dt_weights[weight_year %in% years_with_all_coicops, ]

  # We also have to assume that for a given COICOP code, the index weight years in the weighted consumption table are exactly the same as the index value years in the CPI table!
  # Hence,
  for (coicop_code in cpi_coicops) {
    if (!identical(unique(dt_cpi[coicop == coicop_code, year]), unique(dt_weights[coicop == coicop_code, weight_year]))) {
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
    total = length(years_with_all_coicops) - 2, clear = FALSE
  )
  pb$tick(0)

  categories <- unique(dt_weights$category)

  for (y in years_with_all_coicops[3:length(years_with_all_coicops)]) {
    # This can be further optimised since each COICOP code is independent
    for (j in cpi_coicops) {
      # Constant values
      p_y1_12 <- sum(dt_cpi[month == 12 &
        year == y - 1, value])
      p_y2_12 <- sum(dt_cpi[month == 12 &
        year == y - 2, value])
      p_y1_12_j <- dt_cpi[coicop == j &
        month == 12 &
        year == y - 1, value]
      p_y2_12_j <- dt_cpi[coicop == j &
        month == 12 &
        year == y - 2, value]

      # Calculate HBS weights across all categories
      w_y1_j_q <- dt_weights[coicop == j &
        weight_year == y - 1, .(category, weighted_consumption)]
      data.table::setkey(w_y1_j_q, category)
      w_y2_j_q <- dt_weights[coicop == j &
        weight_year == y - 2, .(category, weighted_consumption)]

      # Calculate index weights across all months
      # (not necessarily 12 for the latest year)
      p_y1_m <- dt_cpi[year == y - 1, sum(value), by = .(month)]
      data.table::setkey(p_y1_m, month)
      p_y1_m_j <- dt_cpi[coicop == j &
        year == y - 1, .(month, value)]
      p_y_m_j <- dt_cpi[coicop == j &
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

  return(contrib2)
}

# Function to check if a weight_year has all unique COICOP codes
has_all_coicop <- function(data, year, coicops) {
  coicop_in_year <- unique(data$coicop[data$year == year])
  all(coicops %in% coicop_in_year)
}
