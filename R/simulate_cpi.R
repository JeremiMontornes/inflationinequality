#' Simulate CPI data
#'
#' @description
#' `simulate_cpi` modifies the price index of specified product categories under specified conditions. For example, you may want to simulate the price index of petrol to be increased by 20% from April 2022 to November 2022.
#'
#' The simulation is computed on unchained item indices, following the same
#' chain-linking convention as [calculate_price_indices()]. Item indices are
#' first converted to unchained annual price relatives with [hicp::unchain()].
#' The requested counterfactual level is then translated back into unchained
#' movements, the product basket is optionally re-aggregated with annual index
#' weights, and simulated levels are rebuilt with [hicp::chain()].
#'
#' The formula to calculate the simulated level during the simulated period is:
#' sim_index_t = index_starttime * (1 + shock)
#'
#' The product basket is recalculated as a weighted Laspeyres aggregate of
#' unchained item movements before chain-linking.
#'
#' @param cpi_obj a "cpi" object.
#' @param simulations a data.frame or data.table
#' @param recalculate_price_basket flag to recalculate the price index of the product basket
#' @param recode_ecoicop2_to_ecoicop1 whether to map ECOICOP v2 HICP item
#'   codes back to ECOICOP v1-style codes before applying simulations and
#'   matching index weights.
#' @return a "cpi" object with the simulated CPI data.
#'
#' @details
#' `simulations` will have the following component:
#'
#' - `coicop`: the COICOP code
#' - `shock`: the change in the COICOP code's CPI by percentage, must be at least -100.
#' - `start_year`: start year
#' - `start_month`: start month
#' - `end_year`: end year
#' - `end_month`: end month
#'
#' @importFrom data.table :=
#' @export
simulate_cpi <- function(cpi_obj, simulations, recalculate_price_basket = FALSE,
                         recode_ecoicop2_to_ecoicop1 = FALSE) {
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

  simulations <- data.table::as.data.table(data.table::copy(simulations))
  simulations[, `:=`(
    coicop = as.character(coicop),
    start_date = as.Date(ISOdate(start_year, start_month, 1)),
    end_date = as.Date(ISOdate(end_year, end_month, 1))
  )]

  if (any(simulations$shock < -100)) {
    stop("All shock values must be at least -100")
  }

  if (!is.logical(recode_ecoicop2_to_ecoicop1) ||
      length(recode_ecoicop2_to_ecoicop1) != 1 ||
      is.na(recode_ecoicop2_to_ecoicop1)) {
    stop("'recode_ecoicop2_to_ecoicop1' must be TRUE or FALSE.")
  }

  if (isTRUE(recode_ecoicop2_to_ecoicop1)) {
    cpi_obj <- recode_cpi_ecoicop2_to_ecoicop1(cpi_obj, target_level = cpi_obj$level)
  }

  simulated_price_dt <- simulate_cpi_item_indices(cpi_obj$dt, simulations)
  simulated_dt <- simulated_price_dt[, .(series_name, coicop, value, year, month)]

  new_dt_basket <- if (isTRUE(recalculate_price_basket)) {
    simulate_cpi_basket(
      cpi_obj = cpi_obj,
      simulated_price_dt = simulated_price_dt,
      simulations = simulations,
      recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
    )
  } else {
    cpi_obj$dt_basket
  }

  cpi(
    dt = simulated_dt,
    dt_basket = new_dt_basket,
    country = cpi_obj$country,
    level = cpi_obj$level
  )
}

simulate_cpi_item_indices <- function(price_dt, simulations) {
  price_dt <- data.table::copy(price_dt)
  price_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(price_dt, coicop, date)
  price_dt[, dec_ratio := hicp::unchain(x = value, t = date), by = coicop]
  price_dt[, chain_index := hicp::chain(x = dec_ratio, t = date, by = 12), by = coicop]
  price_dt[, simulated_chain_index := chain_index]
  price_dt[, direct_simulated_value := NA_real_]

  for (i in seq_len(nrow(simulations))) {
    sim <- simulations[i]
    rows_to_modify <- price_dt[
      coicop == sim$coicop &
        date >= sim$start_date &
        date <= sim$end_date
    ]

    if (nrow(rows_to_modify) == 0) {
      warning(paste("No data found for COICOP", sim$coicop, "in the specified date range"))
      next
    }

    simulable_rows <- rows_to_modify[!is.na(chain_index)]
    if (nrow(simulable_rows) == 0) {
      index_starttime <- rows_to_modify[which.min(date), value]
      simulated_value <- index_starttime * (1 + sim$shock / 100)
      price_dt[
        coicop == sim$coicop &
          date >= sim$start_date &
          date <= sim$end_date,
        direct_simulated_value := simulated_value
      ]
      next
    }

    index_starttime <- simulable_rows[which.min(date), chain_index]
    simulated_value <- index_starttime * (1 + sim$shock / 100)
    price_dt[
      coicop == sim$coicop &
        date >= min(simulable_rows$date) &
        date <= sim$end_date,
      simulated_chain_index := simulated_value
    ]
  }

  price_dt[
    ,
    simulated_dec_ratio := hicp::unchain(x = simulated_chain_index, t = date),
    by = coicop
  ]
  price_dt[
    ,
    simulated_chain_index := hicp::chain(x = simulated_dec_ratio, t = date, by = 12),
    by = coicop
  ]
  price_dt[, value := scale_chained_index_to_original(
    chain_index = simulated_chain_index,
    original_value = value
  ), by = coicop]
  price_dt[!is.na(direct_simulated_value), value := direct_simulated_value]
  price_dt[]
}

simulate_cpi_basket <- function(cpi_obj, simulated_price_dt, simulations,
                                recode_ecoicop2_to_ecoicop1 = FALSE) {
  hicp_level <- if (isTRUE(recode_ecoicop2_to_ecoicop1)) {
    min(cpi_obj$level + 1, 3)
  } else {
    cpi_obj$level
  }
  weights <- load_index_weights(
    cpi_obj$country,
    hicp_level,
    start_year = simulations[, min(start_year)],
    end_year = simulations[, max(end_year)]
  )
  if (isTRUE(recode_ecoicop2_to_ecoicop1)) {
    weights <- recode_index_weights_ecoicop2_to_ecoicop1(weights, target_level = cpi_obj$level)
  }

  total_weights <- data.table::copy(weights$dt)
  if ("weight_year" %in% names(total_weights)) {
    data.table::setnames(total_weights, "weight_year", "year")
  }

  coicops <- simulated_price_dt[, unique(coicop)]
  total_weights <- total_weights[coicop %in% coicops]
  total_weights[, weight := weight * 100 / sum(weight), by = year]

  total_data <- merge(
    simulated_price_dt,
    total_weights,
    by = c("coicop", "year")
  )

  if (nrow(total_data) == 0) {
    stop("No common COICOP-year observations between simulated CPI data and index weights.")
  }

  basket_dt <- total_data[
    !is.na(simulated_dec_ratio) & !is.na(weight),
    .(laspeyres = hicp::laspeyres(x = simulated_dec_ratio, w0 = weight)),
    by = .(year, month, date)
  ]
  data.table::setorder(basket_dt, date)
  basket_dt[, simulated_chain_index := hicp::chain(x = laspeyres, t = date, by = 12)]

  original_basket <- data.table::copy(cpi_obj$dt_basket)
  original_basket[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  basket_dt <- merge(
    original_basket,
    basket_dt[, .(year, month, simulated_chain_index)],
    by = c("year", "month"),
    all.x = TRUE
  )
  data.table::setorder(basket_dt, date)
  basket_dt[, value := scale_chained_index_to_original(
    chain_index = simulated_chain_index,
    original_value = value
  )]
  basket_dt[, .(series_name, value, year, month)]
}

scale_chained_index_to_original <- function(chain_index, original_value) {
  out <- original_value
  anchor <- which(!is.na(chain_index) & !is.na(original_value))[1]
  if (is.na(anchor)) {
    return(out)
  }
  out[!is.na(chain_index)] <- chain_index[!is.na(chain_index)] *
    original_value[anchor] / chain_index[anchor]
  out
}
