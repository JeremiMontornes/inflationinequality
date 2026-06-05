#' Simulate CPI counterfactuals from simple policy parameters
#'
#' @description
#' `simulate_cpi_counterfactual()` applies manually specified counterfactual
#' policy parameters to one CPI item index. It is designed for cases where the
#' policy effect can be represented as a monthly price ratio, such as VAT cuts,
#' unit subsidies, tariff ratios, or an externally supplied counterfactual
#' index.
#'
#' @param cpi Optional `"cpi"` object. If omitted, the function looks for an
#'   object named `cpi` or `cpi_obj` in the calling environment.
#' @param coicop COICOP code to simulate.
#' @param type Counterfactual type. One of `"vat"`, `"ratio"`,
#'   `"unit_subsidy"`, or `"index"`.
#' @param start,end Start and end month of the counterfactual window. Values can
#'   be `"YYYY-MM"` strings or `Date` objects.
#' @param old_rate,new_rate VAT rates for `type = "vat"`, expressed as
#'   decimals. For example, use `0.21` and `0.06`, not `21` and `6`.
#' @param ratio Counterfactual-to-observed price ratio for `type = "ratio"`.
#' @param elasticity Elasticity applied to `ratio` for tariff pass-through
#'   cases. Defaults to `1`.
#' @param subsidy Unit subsidy to remove from the observed price for
#'   `type = "unit_subsidy"`.
#' @param unit_price Observed unit price for `type = "unit_subsidy"`. It can be
#'   a scalar or a data frame/data table with columns `date` and `unit_price`.
#' @param counterfactual_index Counterfactual index for `type = "index"`. It can
#'   be a scalar or a data frame/data table with columns `date` and
#'   `counterfactual_index`.
#' @param recalculate_price_basket Whether to recalculate the aggregate price
#'   basket after changing item indices.
#' @param recode_ecoicop2_to_ecoicop1 Whether to map ECOICOP v2 HICP item codes
#'   back to ECOICOP v1-style codes before applying the counterfactual.
#'
#' @return A `"cpi"` object with counterfactual CPI item indices.
#'
#' @examples
#' dt <- data.table::data.table(
#'   series_name = "CPI",
#'   coicop = "0451",
#'   value = 100,
#'   year = rep(2022, 12),
#'   month = 1:12
#' )
#' dt_basket <- data.table::data.table(
#'   series_name = "CPI",
#'   value = 100,
#'   year = rep(2022, 12),
#'   month = 1:12
#' )
#' cpi <- cpi(dt, dt_basket, "FR", 3)
#' simulate_cpi_counterfactual(
#'   cpi,
#'   coicop = "0451",
#'   type = "vat",
#'   start = "2022-03",
#'   end = "2022-12",
#'   old_rate = 0.21,
#'   new_rate = 0.06
#' )
#'
#' @importFrom data.table :=
#' @export
simulate_cpi_counterfactual <- function(cpi = NULL,
                                        coicop,
                                        type = c("vat", "ratio", "unit_subsidy", "index"),
                                        start,
                                        end,
                                        old_rate = NULL,
                                        new_rate = NULL,
                                        ratio = NULL,
                                        elasticity = 1,
                                        subsidy = NULL,
                                        unit_price = NULL,
                                        counterfactual_index = NULL,
                                        recalculate_price_basket = FALSE,
                                        recode_ecoicop2_to_ecoicop1 = FALSE) {
  if (is.null(cpi)) {
    cpi <- find_calling_cpi()
  }
  if (!inherits(cpi, "cpi")) {
    stop("cpi must be a 'cpi' object")
  }
  if (missing(coicop) || length(coicop) != 1 || is.na(coicop)) {
    stop("coicop must be a single COICOP code")
  }
  if (!is.logical(recalculate_price_basket) ||
      length(recalculate_price_basket) != 1 ||
      is.na(recalculate_price_basket)) {
    stop("'recalculate_price_basket' must be TRUE or FALSE.")
  }
  if (!is.logical(recode_ecoicop2_to_ecoicop1) ||
      length(recode_ecoicop2_to_ecoicop1) != 1 ||
      is.na(recode_ecoicop2_to_ecoicop1)) {
    stop("'recode_ecoicop2_to_ecoicop1' must be TRUE or FALSE.")
  }

  if (isTRUE(recode_ecoicop2_to_ecoicop1)) {
    cpi <- recode_cpi_ecoicop2_to_ecoicop1(cpi, target_level = cpi$level)
  }

  type <- match.arg(type)
  policy <- build_cpi_counterfactual_policy(
    coicop = as.character(coicop),
    type = type,
    start = start,
    end = end,
    old_rate = old_rate,
    new_rate = new_rate,
    ratio = ratio,
    elasticity = elasticity,
    subsidy = subsidy,
    unit_price = unit_price,
    counterfactual_index = counterfactual_index
  )

  simulated_price_dt <- simulate_cpi_counterfactual_item_indices(cpi$dt, policy)
  simulated_dt <- simulated_price_dt[, .(series_name, coicop, value, year, month)]

  new_dt_basket <- if (isTRUE(recalculate_price_basket)) {
    simulate_cpi_counterfactual_basket(
      cpi = cpi,
      simulated_price_dt = simulated_price_dt,
      policy = policy,
      recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
    )
  } else {
    cpi$dt_basket
  }

  validate_cpi(new_cpi(
    dt = simulated_dt,
    dt_basket = new_dt_basket,
    country = cpi$country,
    level = cpi$level
  ))
}

find_calling_cpi <- function() {
  env <- parent.frame(2)
  if (exists("cpi", envir = env, inherits = TRUE)) {
    obj <- get("cpi", envir = env, inherits = TRUE)
    if (inherits(obj, "cpi")) {
      return(obj)
    }
  }
  if (exists("cpi_obj", envir = env, inherits = TRUE)) {
    obj <- get("cpi_obj", envir = env, inherits = TRUE)
    if (inherits(obj, "cpi")) {
      return(obj)
    }
  }
  stop("No 'cpi' object was supplied and no object named 'cpi' or 'cpi_obj' was found.")
}

build_cpi_counterfactual_policy <- function(coicop,
                                            type,
                                            start,
                                            end,
                                            old_rate,
                                            new_rate,
                                            ratio,
                                            elasticity,
                                            subsidy,
                                            unit_price,
                                            counterfactual_index) {
  start_date <- parse_counterfactual_month(start, "start")
  end_date <- parse_counterfactual_month(end, "end")
  if (end_date < start_date) {
    stop("end must be greater than or equal to start")
  }
  dates <- seq(start_date, end_date, by = "1 month")
  policy <- data.table::data.table(
    coicop = coicop,
    date = dates,
    policy_ratio = NA_real_,
    counterfactual_index = NA_real_
  )

  if (type == "vat") {
    old_rate <- validate_scalar_number(old_rate, "old_rate")
    new_rate <- validate_scalar_number(new_rate, "new_rate")
    if (old_rate <= -1 || new_rate <= -1) {
      stop("old_rate and new_rate must be greater than -1")
    }
    policy[, policy_ratio := (1 + old_rate) / (1 + new_rate)]
  } else if (type == "ratio") {
    ratio <- validate_scalar_number(ratio, "ratio")
    elasticity <- validate_scalar_number(elasticity, "elasticity")
    if (ratio <= 0) {
      stop("ratio must be strictly positive")
    }
    policy[, policy_ratio := ratio^elasticity]
  } else if (type == "unit_subsidy") {
    subsidy <- validate_scalar_number(subsidy, "subsidy")
    prices <- normalize_monthly_counterfactual_input(
      unit_price,
      value_col = "unit_price",
      target_dates = dates
    )
    policy <- merge(policy[, .(coicop, date, counterfactual_index)], prices, by = "date", all.x = TRUE)
    if (any(is.na(policy$unit_price))) {
      stop("unit_price must provide one value for every month in the counterfactual window")
    }
    if (any(policy$unit_price <= 0)) {
      stop("unit_price values must be strictly positive")
    }
    policy[, policy_ratio := (unit_price + subsidy) / unit_price]
    policy[, unit_price := NULL]
  } else if (type == "index") {
    index <- normalize_monthly_counterfactual_input(
      counterfactual_index,
      value_col = "counterfactual_index",
      target_dates = dates
    )
    policy <- merge(policy[, .(coicop, date, policy_ratio)], index, by = "date", all.x = TRUE)
    if (any(is.na(policy$counterfactual_index))) {
      stop("counterfactual_index must provide one value for every month in the counterfactual window")
    }
  }

  data.table::setcolorder(policy, c("coicop", "date", "policy_ratio", "counterfactual_index"))
  policy[]
}

parse_counterfactual_month <- function(x, arg) {
  if (inherits(x, "Date")) {
    return(as.Date(sprintf("%s-01", format(x, "%Y-%m"))))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop(arg, " must be a Date or a single 'YYYY-MM' string")
  }
  if (!grepl("^[0-9]{4}-[0-9]{2}$", x)) {
    stop(arg, " must use the 'YYYY-MM' format")
  }
  as.Date(paste0(x, "-01"))
}

validate_scalar_number <- function(x, arg) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !is.numeric(x)) {
    stop(arg, " must be a single numeric value")
  }
  x
}

normalize_monthly_counterfactual_input <- function(x, value_col, target_dates) {
  if (is.null(x)) {
    stop(value_col, " must be supplied")
  }
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    out <- data.table::data.table(date = target_dates, value = x)
    data.table::setnames(out, "value", value_col)
    return(out)
  }
  if (!is.data.frame(x) && !data.table::is.data.table(x)) {
    stop(value_col, " must be a scalar or a data.frame/data.table")
  }
  out <- data.table::as.data.table(data.table::copy(x))
  if (!all(c("date", value_col) %in% names(out))) {
    stop(value_col, " table must contain columns 'date' and '", value_col, "'")
  }
  out[, date := parse_counterfactual_date_column(date)]
  out <- out[, .(date, value = as.numeric(get(value_col)))]
  data.table::setnames(out, "value", value_col)
  out
}

parse_counterfactual_date_column <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(sprintf("%s-01", format(x, "%Y-%m"))))
  }
  if (is.character(x)) {
    is_month <- grepl("^[0-9]{4}-[0-9]{2}$", x)
    out <- rep(as.Date(NA), length(x))
    out[!is_month] <- as.Date(x[!is_month])
    out[is_month] <- as.Date(paste0(x[is_month], "-01"))
    return(as.Date(sprintf("%s-01", format(out, "%Y-%m"))))
  }
  stop("date must be a Date column or character values formatted as 'YYYY-MM' or 'YYYY-MM-DD'")
}

simulate_cpi_counterfactual_item_indices <- function(price_dt, policy) {
  price_dt <- data.table::copy(price_dt)
  price_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(price_dt, coicop, date)
  price_dt[, dec_ratio := hicp::unchain(x = value, t = date), by = coicop]
  price_dt[, chain_index := hicp::chain(x = dec_ratio, t = date, by = 12), by = coicop]
  price_dt[, simulated_chain_index := chain_index]
  price_dt[, direct_counterfactual_value := NA_real_]

  coicop_code <- unique(policy$coicop)
  rows_to_modify <- price_dt[
    coicop == coicop_code &
      date >= min(policy$date) &
      date <= max(policy$date)
  ]
  if (nrow(rows_to_modify) == 0) {
    warning(paste("No data found for COICOP", coicop_code, "in the specified date range"))
    return(price_dt)
  }

  price_dt <- merge(
    price_dt,
    policy,
    by = c("coicop", "date"),
    all.x = TRUE
  )
  data.table::setorder(price_dt, coicop, date)

  price_dt[
    !is.na(policy_ratio),
    counterfactual_index := value * policy_ratio
  ]
  price_dt[
    !is.na(counterfactual_index) & (is.na(chain_index) | is.na(value) | value <= 0),
    direct_counterfactual_value := counterfactual_index
  ]
  price_dt[
    !is.na(counterfactual_index) & !is.na(value) & value > 0 & !is.na(chain_index),
    simulated_chain_index := chain_index * counterfactual_index / value
  ]
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
  price_dt[!is.na(direct_counterfactual_value), value := direct_counterfactual_value]
  price_dt[]
}

simulate_cpi_counterfactual_basket <- function(cpi,
                                               simulated_price_dt,
                                               policy,
                                               recode_ecoicop2_to_ecoicop1 = FALSE) {
  hicp_level <- if (isTRUE(recode_ecoicop2_to_ecoicop1)) {
    min(cpi$level + 1, 3)
  } else {
    cpi$level
  }
  weights <- load_index_weights(
    cpi$country,
    hicp_level,
    start_year = as.integer(format(min(policy$date), "%Y")),
    end_year = as.integer(format(max(policy$date), "%Y"))
  )
  if (isTRUE(recode_ecoicop2_to_ecoicop1)) {
    weights <- recode_index_weights_ecoicop2_to_ecoicop1(weights, target_level = cpi$level)
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

  original_basket <- data.table::copy(cpi$dt_basket)
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
