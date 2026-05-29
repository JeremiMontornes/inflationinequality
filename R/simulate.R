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
#' - `coicop`: the COICOP code
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

#' Simulate an input-output supply shock and inflation inequality effects
#'
#' @description
#' `simulate_shock()` propagates a sectoral unit-cost shock through an
#' input-output price model,
#' `delta_p = solve(I - t(A), s)`, then aggregates the resulting sectoral price
#' changes with household consumption weights. It reports the average inflation
#' effect and inflation gaps for income, age, and area groups, by country and for
#' an aggregate such as the euro area.
#'
#' @param A square input-output technical-coefficient matrix. Rows are input
#'   sectors and columns are producing sectors. Row and column names must match.
#' @param nodes data.frame with one row per row/column of `A`. Required columns
#'   are `node`, `country`, and `sector`.
#' @param shock numeric scalar or named vector. If scalar, it is applied to all
#'   rows selected by `shock_sectors`. If named, names must match node names.
#' @param shock_sectors sectors receiving the direct shock. Defaults to common
#'   FIGARO energy sectors: mining and quarrying, coke/refined petroleum, and
#'   electricity/gas.
#' @param shock_scope scope used when `shock` is a scalar. `"all"` shocks all
#'   selected sectors in the matrix, `"domestic"` shocks selected sectors in the
#'   reporting country only, and `"imported"` shocks selected sectors outside
#'   the reporting country. `"imported"` is useful for imported energy input
#'   price shocks.
#' @param bridge data.frame mapping consumption COICOP codes to NACE/FIGARO
#'   sectors. Required columns are `coicop`, `sector`, and optionally `share`.
#'   If `share` is missing, equal shares are used within each COICOP.
#' @param countries country codes to report. Defaults to all countries in
#'   `nodes`.
#' @param aggregate_geo name of the aggregate row. Defaults to `"EA20"`.
#' @param country_weights optional data.frame with columns `country` and
#'   `country_weight`, used to aggregate countries into `aggregate_geo`.
#' @param total_weights optional data.frame of average consumption weights with
#'   columns `country`, `coicop`, and `weight`.
#' @param group_weights named list of data.frames for inequality groups. Each
#'   element name is an outcome name, for example
#'   `inflation_gap_revenu`, `inflation_gap_age`, or `inflation_gap_area`.
#'   Each data.frame must contain `country`, `coicop`, `category`, and `weight`.
#' @param gap_categories optional named list. Each element is a length-two
#'   character vector defining the low/reference group and high/comparison
#'   group for the matching `group_weights` element.
#' @param output_scale multiplier applied to inflation effects. Use `100` to
#'   express a 0.01 price change as 1 percentage point. Defaults to `100`.
#'
#' @returns A data.table with one row per country plus the aggregate row.
#'
#' @export
simulate_shock <- function(A,
                           nodes,
                           shock = 0.10,
                           shock_sectors = c("B", "C19", "D35"),
                           shock_scope = c("all", "domestic", "imported"),
                           bridge,
                           countries = NULL,
                           aggregate_geo = "EA20",
                           country_weights = NULL,
                           total_weights = NULL,
                           group_weights = list(),
                           gap_categories = list(),
                           output_scale = 100) {
  A <- as.matrix(A)
  if (!is.numeric(A) || nrow(A) != ncol(A)) {
    stop("A must be a square numeric matrix.", call. = FALSE)
  }
  if (is.null(rownames(A)) || is.null(colnames(A)) ||
      !setequal(rownames(A), colnames(A))) {
    stop("A must have matching row and column names.", call. = FALSE)
  }
  A <- A[rownames(A), rownames(A), drop = FALSE]
  shock_scope <- match.arg(shock_scope)

  nodes <- data.table::as.data.table(nodes)
  required_node_cols <- c("node", "country", "sector")
  if (!all(required_node_cols %in% names(nodes))) {
    stop("nodes must contain columns: node, country, sector.", call. = FALSE)
  }
  nodes[, `:=`(
    node = as.character(node),
    country = toupper(as.character(country)),
    sector = as.character(sector)
  )]
  nodes <- nodes[match(rownames(A), node)]
  if (anyNA(nodes$node)) {
    stop("nodes must contain one row for every row/column name of A.", call. = FALSE)
  }

  if (is.null(countries)) {
    countries <- sort(unique(nodes$country))
  }
  countries <- toupper(countries)

  leontief_price <- solve(diag(nrow(A)) - t(A))

  shock_effects <- function(country_i = NULL) {
    s <- build_shock_vector(nodes, shock, shock_sectors, shock_scope, country_i)
    names(s) <- nodes$node
    delta_total <- as.numeric(leontief_price %*% s)
    names(delta_total) <- nodes$node
    list(
      total = delta_total,
      direct = s,
      indirect = delta_total - s
    )
  }

  common_effects <- if (identical(shock_scope, "all") || length(shock) != 1 || !is.null(names(shock))) {
    shock_effects()
  } else {
    NULL
  }

  bridge <- normalize_shock_bridge(bridge)
  total_sector_weights <- prepare_total_sector_weights(
    total_weights = total_weights,
    bridge = bridge,
    countries = countries
  )

  rows <- lapply(countries, function(country_i) {
    effects <- common_effects %||% shock_effects(country_i)
    delta_total <- effects$total
    delta_direct <- effects$direct
    delta_indirect <- effects$indirect
    country_nodes <- nodes[country == country_i, .(node, sector)]
    country_total_weights <- total_sector_weights[country == country_i]
    base_row <- data.table::data.table(
      country = country_i,
      delta_p = weighted_country_effect(country_total_weights, delta_total, country_nodes) * output_scale,
      delta_p_direct = weighted_country_effect(country_total_weights, delta_direct, country_nodes) * output_scale,
      delta_p_indirect = weighted_country_effect(country_total_weights, delta_indirect, country_nodes) * output_scale
    )

    for (outcome in names(group_weights)) {
      group_sector_weights <- prepare_group_sector_weights(
        group_weights[[outcome]],
        bridge = bridge,
        countries = country_i
      )
      cats <- resolve_gap_categories(group_sector_weights, gap_categories[[outcome]])
      low_w <- group_sector_weights[country == country_i & category == cats[1]]
      high_w <- group_sector_weights[country == country_i & category == cats[2]]
      gap_w <- subtract_sector_weights(low_w, high_w)

      base_row[, (outcome) := weighted_country_effect(gap_w, delta_total, country_nodes) * output_scale]
      base_row[, (paste0(outcome, "_direct")) := weighted_country_effect(gap_w, delta_direct, country_nodes) * output_scale]
      base_row[, (paste0(outcome, "_indirect")) := weighted_country_effect(gap_w, delta_indirect, country_nodes) * output_scale]
    }
    base_row
  })
  out <- data.table::rbindlist(rows, fill = TRUE)

  if (!is.null(country_weights) && length(countries) > 1) {
    aggregate_row <- aggregate_shock_results(
      out,
      country_weights = country_weights,
      aggregate_geo = aggregate_geo
    )
    out <- data.table::rbindlist(list(out, aggregate_row), fill = TRUE)
  }

  out[]
}

build_shock_vector <- function(nodes, shock, shock_sectors, shock_scope = "all", country = NULL) {
  if (length(shock) == 1 && is.null(names(shock))) {
    s <- rep(0, nrow(nodes))
    selected <- nodes$sector %in% shock_sectors
    if (identical(shock_scope, "domestic")) {
      if (is.null(country)) stop("country is required when shock_scope = 'domestic'.", call. = FALSE)
      selected <- selected & nodes$country == country
    } else if (identical(shock_scope, "imported")) {
      if (is.null(country)) stop("country is required when shock_scope = 'imported'.", call. = FALSE)
      selected <- selected & nodes$country != country
    }
    s[selected] <- shock
    return(s)
  }

  if (is.null(names(shock))) {
    stop("shock must be a scalar or a named vector.", call. = FALSE)
  }
  s <- rep(0, nrow(nodes))
  matched <- match(names(shock), nodes$node)
  if (anyNA(matched)) {
    stop(
      "shock contains names not present in nodes$node: ",
      paste(names(shock)[is.na(matched)], collapse = ", "),
      call. = FALSE
    )
  }
  s[matched] <- as.numeric(shock)
  s
}

normalize_shock_bridge <- function(bridge) {
  bridge <- data.table::as.data.table(bridge)
  required <- c("coicop", "sector")
  if (!all(required %in% names(bridge))) {
    stop("bridge must contain columns: coicop, sector.", call. = FALSE)
  }
  if (!"share" %in% names(bridge)) {
    bridge[, share := 1 / .N, by = coicop]
  }
  bridge[, `:=`(
    coicop = normalize_coicop_code(coicop),
    sector = as.character(sector),
    share = as.numeric(share)
  )]
  if ("country" %in% names(bridge)) {
    bridge[, country := toupper(as.character(country))]
  }
  bridge <- bridge[!is.na(coicop) & !is.na(sector) & is.finite(share) & share > 0]
  bridge_by <- intersect(c("country", "coicop"), names(bridge))
  bridge[, share := share / sum(share), by = bridge_by]
  bridge[]
}

normalize_coicop_code <- function(x) {
  x <- as.character(x)
  x <- sub("^CP", "", x)
  gsub("[^0-9]", "", x)
}

prepare_total_sector_weights <- function(total_weights, bridge, countries) {
  if (is.null(total_weights)) {
    stop("total_weights must be supplied for simulate_shock().", call. = FALSE)
  }
  total_weights <- data.table::as.data.table(total_weights)
  required <- c("country", "coicop", "weight")
  if (!all(required %in% names(total_weights))) {
    stop("total_weights must contain columns: country, coicop, weight.", call. = FALSE)
  }
  total_weights[, `:=`(
    country = toupper(as.character(country)),
    coicop = normalize_coicop_code(coicop),
    weight = as.numeric(weight)
  )]
  total_weights <- total_weights[country %in% countries]
  coicop_to_sector_weights(total_weights, bridge)
}

prepare_group_sector_weights <- function(weights, bridge, countries) {
  weights <- data.table::as.data.table(weights)
  required <- c("country", "coicop", "category", "weight")
  if (!all(required %in% names(weights))) {
    stop("group_weights entries must contain columns: country, coicop, category, weight.", call. = FALSE)
  }
  weights[, `:=`(
    country = toupper(as.character(country)),
    coicop = normalize_coicop_code(coicop),
    category = as.character(category),
    weight = as.numeric(weight)
  )]
  weights <- weights[country %in% toupper(countries)]
  coicop_to_sector_weights(weights, bridge, by_cols = c("country", "category"))
}

coicop_to_sector_weights <- function(weights, bridge, by_cols = "country") {
  merge_cols <- "coicop"
  if ("country" %in% names(bridge)) {
    merge_cols <- c("country", "coicop")
  }
  out <- merge(weights, bridge, by = merge_cols, allow.cartesian = TRUE)
  out[, sector_weight := weight * share]
  out <- out[, .(weight = sum(sector_weight, na.rm = TRUE)), by = c(by_cols, "sector")]
  out[, weight := weight / sum(weight, na.rm = TRUE), by = by_cols]
  out[]
}

weighted_country_effect <- function(sector_weights, delta, country_nodes) {
  if (nrow(sector_weights) == 0) return(NA_real_)
  country_nodes <- data.table::as.data.table(country_nodes)
  node_names <- country_nodes$node
  node_dt <- data.table::data.table(
    node = node_names,
    sector = country_nodes$sector,
    delta = delta[node_names]
  )
  merged <- merge(sector_weights, node_dt, by = "sector", all.x = TRUE)
  sum(merged$weight * merged$delta, na.rm = TRUE)
}

subtract_sector_weights <- function(low_w, high_w) {
  low_w <- data.table::copy(low_w[, .(sector, low_weight = weight)])
  high_w <- data.table::copy(high_w[, .(sector, high_weight = weight)])
  out <- merge(low_w, high_w, by = "sector", all = TRUE)
  out[is.na(low_weight), low_weight := 0]
  out[is.na(high_weight), high_weight := 0]
  out[, weight := low_weight - high_weight]
  out[, .(sector, weight)]
}

resolve_gap_categories <- function(group_sector_weights, requested = NULL) {
  if (!is.null(requested)) {
    if (length(requested) != 2) {
      stop("Each gap_categories entry must contain exactly two categories.", call. = FALSE)
    }
    return(as.character(requested))
  }
  cats <- unique(group_sector_weights$category)
  if (length(cats) < 2) {
    stop("At least two categories are needed to compute an inflation gap.", call. = FALSE)
  }
  c(cats[1], cats[length(cats)])
}

aggregate_shock_results <- function(out, country_weights, aggregate_geo) {
  country_weights <- data.table::as.data.table(country_weights)
  required <- c("country", "country_weight")
  if (!all(required %in% names(country_weights))) {
    stop("country_weights must contain columns: country, country_weight.", call. = FALSE)
  }
  country_weights[, `:=`(
    country = toupper(as.character(country)),
    country_weight = as.numeric(country_weight)
  )]
  dt <- merge(out, country_weights, by = "country")
  dt[, country_weight := country_weight / sum(country_weight, na.rm = TRUE)]
  value_cols <- setdiff(names(out), "country")
  agg <- dt[, lapply(.SD, function(x) sum(x * country_weight, na.rm = TRUE)), .SDcols = value_cols]
  agg[, country := aggregate_geo]
  data.table::setcolorder(agg, names(out))
  agg[]
}
