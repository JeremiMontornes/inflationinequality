#' Simulate CPI data
#'
#' @description
#' `simulate_cpi` modifies the price index of specified product categories under specified conditions. For example, you may want to simulate the price index of petrol to be increased by 20% from April 2022 to November 2022.
#'
#' The formula to calculate the simulate index at time t is:
#' sim_index_t = index_starttime * (1 + shock)
#'
#' The formula to calculate the price index of the produce basket:
#' Py,m= Py-1,12 * sum(wi * Pi y,m / Pi y-1,12)
#'
#' @param cpi_obj a "cpi" object.
#' @param simulations a data.frame or data.table
#' @param recalculate_price_basket flag to recalculate the price index of the product basket
#' @return a "cpi" object with the simulated CPI data.
#'
#' @details
#' `simulations` will have the following component:
#'
#' - `coicop`: the COICÖP code
#' - `shock`: the change in the COICOP code's CPI by percentage, must be at least -100.
#' - `start_year`: start year
#' - `start_month`: start month
#' - `end_year`: end year
#' - `end_month`: end month
#'
#' @importFrom data.table :=
#' @export
simulate_cpi <- function(cpi_obj, simulations, recalculate_price_basket = FALSE) {
  # Validate input
  if (!inherits(cpi_obj, "cpi")) {
    stop("cpi_obj must be a 'cpi' object")
  }

  if (!is.data.frame(simulations) && !data.table::is.data.table(simulations)) {
    stop("simulations must be a data.frame or data.table")
  }

  required_cols <- c("coicop", "shock", "start_year", "start_month", "end_year", "end_month")
  if (!all(required_cols %in% names(simulations))) {
    stop(paste("simulations must contain columns:", paste(required_cols, collapse = ", ")))
  }

  # Convert to data.table if it's not already
  simulations <- data.table::as.data.table(simulations)

  # Ensure dates are in proper format
  simulations[, ':='(
    start_date = as.Date(ISOdate(start_year, start_month, 1)),
    end_date = as.Date(ISOdate(end_year, end_month, 1))
  )]

  # Validate shock values
  if (any(simulations$shock < -100)) {
    stop("All shock values must be at least -100")
  }

  # Create a copy of the original data
  simulated_dt <- data.table::copy(cpi_obj$dt)

  # Add date column to simulated_dt for easier merging
  simulated_dt[, date := as.Date(paste(year, month, "01", sep = "-"))]

  # Perform simulations
  for (i in 1:nrow(simulations)) {
    sim <- simulations[i]

    # Filter rows to modify
    rows_to_modify <- simulated_dt[coicop == sim$coicop &
                                     date >= sim$start_date &
                                     date <= sim$end_date]

    if (nrow(rows_to_modify) == 0) {
      warning(paste("No data found for COICOP", sim$coicop, "in the specified date range"))
      next
    }

    # Get the index value at the start time
    index_starttime <- rows_to_modify[which.min(date), value]

    # Apply the shock
    rows_to_modify[, value := index_starttime * (1 + sim$shock / 100)]

    # Update the main data table
    simulated_dt[rows_to_modify, on = .(coicop, date), value := i.value]
  }

  # Remove the temporary date column
  simulated_dt[, date := NULL]

  if (recalculate_price_basket) {
    generate_year_month_sequence <- function(dt) {
      # Generate all possible year-month combinations
      all_dates <- data.table::CJ(
        year = dt[, min(start_year)]:dt[, max(end_year)],
        month = 1:12,
        sorted = FALSE
      )
      all_dates[, date := as.Date(paste(year, month, "01", sep = "-"))]

      # Filter dates that are within any of the specified ranges
      result <- all_dates[dt, on = .(date >= start_date, date <= end_date), nomatch = 0L]

      # Select and order the final columns
      result <- result[, .(year, month)]

      return(result)
    }

    basket_rows_to_modify <- generate_year_month_sequence(simulations)

    # Load index weights
    weights <- load_index_weights(cpi_obj$country, cpi_obj$level,
                                  start_year = simulations[, min(start_year)],
                                  end_year = simulations[, max(end_year)])


    # We select products that only exist in the CPI object,
    # in order to recalculate price index of the product basket
    coicops <- cpi_obj$dt[, unique(coicop)]
    weights$dt <- weights$dt[coicop %in% coicops, ]

    # Normalize weights
    weights$dt[, weight := weight * 100 / sum(weight), by = .(year)]

    # copy object, we want to keep unmodified values
    new_dt_basket <- data.table::copy(cpi_obj$dt_basket)

    for (i in 1:nrow(basket_rows_to_modify)) {
      basket_row <- basket_rows_to_modify[i]
      # Throw error if we cannot find the required data
      if (!setequal(coicops, simulated_dt[year == basket_row$year-1, unique(coicop)])
          | nrow(new_dt_basket[year == basket_row$year & month == 12, ]) == 0) {
        stop("Not enough data to calculate new price basket index")
      }
      new_dt_basket[
        year == basket_row$year & month == basket_row$month,
        # This follows the formula in the description
        value := new_dt_basket[year == basket_row$year - 1 & month == 12, value] * sum(weights$dt[year == basket_row$year, weight] * simulated_dt[year == basket_row$year & month == basket_row$month, value] / simulated_dt[year == basket_row$year - 1 & month == 12, value]) / 100
        ]
    }
  }

  # Create a new cpi object with the simulated data
  new_cpi_obj <- cpi(dt = simulated_dt,
                     dt_basket = if (recalculate_price_basket) new_dt_basket else cpi_obj$dt_basket,
                     country = cpi_obj$country,
                     level = cpi_obj$level)

  return(new_cpi_obj)
}
