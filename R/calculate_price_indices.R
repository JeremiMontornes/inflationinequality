#' Calculate price indices by household category
#'
#' @description
#' `calculate_price_indices()` builds monthly chained Laspeyres price indices by
#' household category. It uses HICP item price indices and HICP item weights from
#' Eurostat via the `hicp` package, combined with public semi-aggregated HBS data
#' loaded by [calculate_weights()].
#'
#' @inheritParams calculate_inflation
#' @param base_year year used to rebase the chained index to 100. If `NULL`,
#'   the package uses `2025` as the default presentation base.
#' @param include_total whether to include the official all-items HICP index as
#'   a `"Total"` category.
#' @param formula upper-level index formula. `"laspeyres"` uses the package's
#'   Eurostat-style chained Laspeyres aggregation with annual weights.
#'   `"toernqvist"` uses [hicp::toernqvist()] with previous-year and current-year
#'   category weights as an approximation to a chained Törnqvist index.
#' @param recode_ecoicop2_to_ecoicop1 whether to map ECOICOP v2 HICP item
#'   codes back to ECOICOP v1-style codes before matching them to HBS weights.
#' @param aggregate_geo country-group aggregate used when `country` contains
#'   several countries. The default `"EA20"` loads Eurostat HICP country weights
#'   (`prc_hicp_cow`, `statinfo = "COWEA20"`).
#' @param custom_country_weights optional data frame with columns `country`,
#'   `year`, and `weight` (or `country_weight`) used to aggregate national
#'   indices when `country` contains several countries.
#'
#' When `country = "EA20"`, `level` is forced to `2` because euro-area
#' household-group aggregation is currently supported at COICOP level 2.
#'
#' @section National level 3 HBS:
#' For France, `level = 3` automatically uses bundled INSEE 2017 HBS level-3
#' data for income, age, and residence-area groups when `custom_hbs` is not
#' supplied. For Spain, `level = 3` uses compact INE EPF 2020 microdata-derived
#' HBS objects when available. For Portugal, `level = 3` uses bundled INE IDEF
#' 2015/2016 objects that preserve the published group-specific COICOP level-2
#' totals and allocate them across level-3 classes using the published national
#' four-digit composition. Eurostat public HBS data are generally not
#' available at this granularity, so national HBS sources are required for a
#' genuine 4-digit COICOP calculation.
#'
#' @returns An object of class `"price_indices"` containing:
#' - `dt`: a `data.table` with columns `year`, `month`, `date`, `category`,
#'   `laspeyres`, `chain_laspeyres`, `price_index`, and `annual_rate`.
#' - `country`: 2-digit country code.
#' - `category`: HBS category used to build household groups.
#' - `categories`: ordered category labels.
#' - `level`: COICOP level.
#' - `base_year`: rebasing year.
#'
#' @details
#' The function does not use individual HBS microdata. Category-specific weights
#' are obtained by combining public semi-aggregated HBS expenditure shares with
#' official HICP item weights through [calculate_weights()]. For each household
#' category and month, it aggregates unchained HICP item movements with a
#' Laspeyres formula and then chains the monthly aggregates annually.
#'
#' When `country` contains several country codes, or when `country` is a
#' country-group code such as `"EA20"`, the function first calculates national
#' household-group indices and then aggregates them by date and group with
#' Eurostat HICP country weights. This is the recommended workflow for euro-area
#' household-group indices because HBS income quintiles are defined within each
#' country.
#'
#' @examples
#' \dontrun{
#' indices <- calculate_price_indices("FR", "income", start_year = 2019)
#' indices$dt
#' }
#'
#' @export
calculate_price_indices <- function(country = NULL, category = NULL, level = 2,
                                    start_year = NULL, start_month = NULL,
                                    end_year = NULL, end_month = NULL,
                                    ensure_complete_cpi = FALSE,
                                    custom_cpi = NULL,
                                    custom_index_weights = NULL,
                                    custom_hbs = NULL,
                                    interpolated_hbs = FALSE,
                                    specific_hbs_year = NULL,
                                    france_insee_income_groups = c("decile", "quintile"),
                                    base_year = NULL,
                                    include_total = TRUE,
                                    formula = c("laspeyres", "toernqvist"),
                                    recode_ecoicop2_to_ecoicop1 = TRUE,
                                    aggregate_geo = "EA20",
                                    custom_country_weights = NULL,
                                    weighting_method = c("relative_expenditure", "ras", "additive_qp")) {
  if (is.null(country) && is.null(custom_cpi)) {
    stop("Either 'country' or 'custom_cpi' must be provided.")
  }
  if (is.null(category) && is.null(custom_hbs)) {
    stop("Either 'category' or 'custom_hbs' must be provided.")
  }

  if (!is.null(country)) {
    country <- toupper(country)
  }
  formula <- match.arg(formula)
  weighting_method <- match.arg(weighting_method)
  france_insee_income_groups <- match.arg(france_insee_income_groups)
  requested_base_year <- base_year
  base_year <- base_year %||% default_hicp_base_year()
  calculation_start_year <- if (is.null(start_year)) {
    NULL
  } else {
    min(start_year, base_year, na.rm = TRUE)
  }

  if (identical(country, "EA20") && !identical(as.numeric(level), 2)) {
    warning("country = 'EA20' currently supports only level = 2; using level = 2.")
    level <- 2
  }

  if (!is.null(country) && length(country) == 1 &&
      grepl("^EA[0-9]+$", country) && is.null(custom_cpi)) {
    aggregate_geo <- country
    countries <- if (is.null(custom_country_weights)) {
      unique(load_country_weights(
        aggregate_geo = aggregate_geo,
        start_year = start_year,
        end_year = end_year
      )$country)
    } else {
      unique(normalize_country_weights(custom_country_weights)$country)
    }
    return(calculate_price_indices_country_aggregate(
      countries = countries,
      category = category,
      level = level,
      start_year = start_year,
      start_month = start_month,
      end_year = end_year,
      end_month = end_month,
      ensure_complete_cpi = ensure_complete_cpi,
      interpolated_hbs = interpolated_hbs,
      specific_hbs_year = specific_hbs_year,
      france_insee_income_groups = france_insee_income_groups,
      base_year = requested_base_year,
      include_total = include_total,
      formula = formula,
      recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1,
      aggregate_geo = aggregate_geo,
      custom_country_weights = custom_country_weights,
      weighting_method = weighting_method
    ))
  }

  if (!is.null(country) && length(country) > 1) {
    if (!is.null(custom_cpi) || !is.null(custom_index_weights) || !is.null(custom_hbs)) {
      stop(
        "Multi-country aggregation does not support custom_cpi, ",
        "custom_index_weights, or custom_hbs. Calculate national indices ",
        "separately and aggregate them with aggregate_price_indices_by_country()."
      )
    }
    return(calculate_price_indices_country_aggregate(
      countries = country,
      category = category,
      level = level,
      start_year = start_year,
      start_month = start_month,
      end_year = end_year,
      end_month = end_month,
      ensure_complete_cpi = ensure_complete_cpi,
      interpolated_hbs = interpolated_hbs,
      specific_hbs_year = specific_hbs_year,
      france_insee_income_groups = france_insee_income_groups,
      base_year = requested_base_year,
      include_total = include_total,
      formula = formula,
      recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1,
      aggregate_geo = aggregate_geo,
      custom_country_weights = custom_country_weights,
      weighting_method = weighting_method
    ))
  }

  if (use_france_insee_level3_hbs(country, category, level, custom_hbs)) {
    custom_hbs <- load_france_insee_hbs_level3(
      category = category,
      income_groups = france_insee_income_groups
    )
  }
  if (use_spain_epf_2020_level3_hbs(country, category, level, custom_hbs)) {
    custom_hbs <- load_spain_epf_2020_hbs_level3(category = category)
  }
  if (use_portugal_idef_2015_level3_hbs(country, category, level, custom_hbs)) {
    custom_hbs <- load_portugal_idef_2015_hbs_level3(category = category)
  }

  data_start_year <- if (!is.null(calculation_start_year)) calculation_start_year - 1 else NULL
  hicp_level <- if (recode_ecoicop2_to_ecoicop1) min(level + 1, 3) else level

  cpi_obj <- if (is.null(custom_cpi)) {
    load_cpi(
      country, level = hicp_level,
      start_year = data_start_year, start_month = start_month,
      end_year = end_year, end_month = end_month
    )
  } else {
    custom_cpi
  }

  if (ensure_complete_cpi) {
    cpi_obj <- correct_cpi(cpi_obj)
  }

  start_year_out <- if (is.null(start_year)) cpi_obj$start_year else start_year
  end_year_out <- if (is.null(end_year)) cpi_obj$end_year else end_year
  calculation_start_year <- min(start_year_out, base_year, na.rm = TRUE)

  index_weights_obj <- if (is.null(custom_index_weights)) {
    load_index_weights(
      country %||% cpi_obj$country,
      level = hicp_level,
      start_year = calculation_start_year,
      end_year = end_year_out
    )
  } else {
    custom_index_weights
  }

  if (recode_ecoicop2_to_ecoicop1) {
    cpi_obj <- recode_cpi_ecoicop2_to_ecoicop1(
      cpi_obj,
      target_level = level,
      index_weights_obj = index_weights_obj
    )
    index_weights_obj <- recode_index_weights_ecoicop2_to_ecoicop1(index_weights_obj, target_level = level)
  }

  price_dt <- data.table::copy(cpi_obj$dt)
  price_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(price_dt, coicop, date)
  price_dt[, dec_ratio := hicp::unchain(x = value, t = date), by = coicop]

  if (use_euro_area_fast_total(country, custom_hbs)) {
    if (!isTRUE(include_total)) {
      stop("country = 'EA' without custom_hbs can only return include_total = TRUE.")
    }
    index_dt <- calculate_total_price_index_dt(
      price_dt = price_dt,
      index_weights_obj = index_weights_obj,
      base_year = base_year,
      formula = formula
    )
    index_dt <- index_dt[
      (year > start_year_out | (year == start_year_out & month >= (start_month %||% 1))) &
        (year < end_year_out | (year == end_year_out & month <= (end_month %||% 12)))
    ]
    data.table::setorder(index_dt, category, date)

    return(structure(
      list(
        dt = index_dt,
        country = country %||% cpi_obj$country,
        category = category,
        categories = "Total",
        level = level,
        start_year = min(index_dt$year),
        start_month = min(index_dt[year == min(year), month]),
        end_year = max(index_dt$year),
        end_month = max(index_dt[year == max(year), month]),
        base_year = base_year,
        formula = formula
      ),
      class = "price_indices"
    ))
  }

  weights <- calculate_weights(
    country = country %||% cpi_obj$country,
    category = category %||% custom_hbs$category,
    level = level,
    start_year = if (is.null(custom_index_weights) && is.null(custom_hbs)) calculation_start_year else NULL,
    end_year = if (is.null(custom_index_weights) && is.null(custom_hbs)) end_year_out else NULL,
    custom_index_weights = index_weights(
      data.table::copy(index_weights_obj$dt),
      country = index_weights_obj$country,
      level = index_weights_obj$level,
      base_total = index_weights_obj$base_total
    ),
    custom_hbs = custom_hbs,
    interpolated_hbs = interpolated_hbs,
    specific_hbs_year = specific_hbs_year,
    france_insee_income_groups = france_insee_income_groups,
    weighting_method = weighting_method
  )

  weights_dt <- data.table::copy(weights$dt)
  data.table::setnames(weights_dt, "year", "hbs_year")
  data.table::setnames(weights_dt, "weight_year", "year")

  hicp_data <- merge(
    price_dt,
    weights_dt,
    by = c("coicop", "year"),
    allow.cartesian = TRUE
  )

  if (nrow(hicp_data) == 0) {
    stop("No common COICOP-year observations between CPI data and category weights.")
  }

  if (formula == "toernqvist") {
    weights_dt_previous <- data.table::copy(weights_dt)
    weights_dt_previous[, year := year + 1L]
    weights_dt_previous <- weights_dt_previous[
      ,
      .(coicop, year, category, weighted_consumption_previous = weighted_consumption)
    ]
    hicp_data <- merge(
      hicp_data,
      weights_dt_previous,
      by = c("coicop", "year", "category"),
      all.x = TRUE
    )
    hicp_data[
      is.na(weighted_consumption_previous),
      weighted_consumption_previous := weighted_consumption
    ]

    index_dt <- hicp_data[
      !is.na(dec_ratio) & !is.na(weighted_consumption) &
        !is.na(weighted_consumption_previous),
      .(laspeyres = hicp::toernqvist(
        x = dec_ratio,
        w0 = weighted_consumption_previous,
        wt = weighted_consumption
      )),
      by = .(category, year, month, date)
    ]
  } else {
    index_dt <- hicp_data[
      !is.na(dec_ratio) & !is.na(weighted_consumption),
      .(laspeyres = hicp::laspeyres(x = dec_ratio, w0 = weighted_consumption)),
      by = .(category, year, month, date)
    ]
  }

  data.table::setorder(index_dt, category, date)
  index_dt[, chain_laspeyres := hicp::chain(x = laspeyres, t = date, by = 12),
           by = category]
  index_dt[, price_index := rebase_or_first_available(
    x = chain_laspeyres,
    t = date,
    t.ref = as.character(base_year)
  ), by = category]
  index_dt[, annual_rate := hicp::rates(x = price_index, t = date, type = "year"),
           by = category]

  index_dt <- index_dt[
    (year > start_year_out | (year == start_year_out & month >= (start_month %||% 1))) &
      (year < end_year_out | (year == end_year_out & month <= (end_month %||% 12)))
  ]

  if (include_total) {
    total_weights <- data.table::copy(index_weights_obj$dt)
    if ("weight_year" %in% names(total_weights)) {
      data.table::setnames(total_weights, "weight_year", "year")
    }
    total_data <- merge(price_dt, total_weights, by = c("coicop", "year"))
    if (formula == "toernqvist") {
      total_weights_previous <- data.table::copy(total_weights)
      total_weights_previous[, year := year + 1L]
      data.table::setnames(total_weights_previous, "weight", "weight_previous")
      total_weights_previous <- total_weights_previous[, .(coicop, year, weight_previous)]
      total_data <- merge(total_data, total_weights_previous, by = c("coicop", "year"), all.x = TRUE)
      total_data[is.na(weight_previous), weight_previous := weight]
      total_dt <- total_data[
        !is.na(dec_ratio) & !is.na(weight) & !is.na(weight_previous),
        .(laspeyres = hicp::toernqvist(x = dec_ratio, w0 = weight_previous, wt = weight)),
        by = .(year, month, date)
      ]
    } else {
      total_dt <- total_data[
        !is.na(dec_ratio) & !is.na(weight),
        .(laspeyres = hicp::laspeyres(x = dec_ratio, w0 = weight)),
        by = .(year, month, date)
      ]
    }
    data.table::setorder(total_dt, date)
    total_dt[, category := "Total"]
    total_dt[, chain_laspeyres := hicp::chain(x = laspeyres, t = date, by = 12)]
    total_dt[, price_index := rebase_or_first_available(
      x = chain_laspeyres,
      t = date,
      t.ref = as.character(base_year)
    )]
    total_dt[, annual_rate := hicp::rates(x = price_index, t = date, type = "year")]
    total_dt <- total_dt[
      (year > start_year_out | (year == start_year_out & month >= (start_month %||% 1))) &
        (year < end_year_out | (year == end_year_out & month <= (end_month %||% 12))),
      .(category, year, month, date, laspeyres, chain_laspeyres, price_index, annual_rate)
    ]
    index_dt <- data.table::rbindlist(
      list(index_dt[, .(category, year, month, date, laspeyres, chain_laspeyres, price_index, annual_rate)],
           total_dt),
      use.names = TRUE
    )
  }

  data.table::setorder(index_dt, category, date)

  structure(
    list(
      dt = index_dt,
      country = country %||% cpi_obj$country,
      category = category %||% custom_hbs$category,
      categories = if (include_total) c(weights$categories, "Total") else weights$categories,
      level = level,
      start_year = min(index_dt$year),
      start_month = min(index_dt[year == min(year), month]),
      end_year = max(index_dt$year),
      end_month = max(index_dt[year == max(year), month]),
        base_year = base_year,
        formula = formula,
        weighting_method = weighting_method
      ),
    class = "price_indices"
  )
}

calculate_price_indices_country_aggregate <- function(countries, category, level,
                                                      start_year = NULL,
                                                      start_month = NULL,
                                                      end_year = NULL,
                                                      end_month = NULL,
                                                      ensure_complete_cpi = FALSE,
                                                      interpolated_hbs = FALSE,
                                                      specific_hbs_year = NULL,
                                                      france_insee_income_groups = c("decile", "quintile"),
                                                      base_year = NULL,
                                                      include_total = TRUE,
                                                      formula = c("laspeyres", "toernqvist"),
                                                      recode_ecoicop2_to_ecoicop1 = TRUE,
                                                      aggregate_geo = "EA20",
                                                      custom_country_weights = NULL,
                                                      weighting_method = c("relative_expenditure", "ras", "additive_qp")) {
  countries <- toupper(countries)
  formula <- match.arg(formula)
  weighting_method <- match.arg(weighting_method)
  france_insee_income_groups <- match.arg(france_insee_income_groups)
  output_start_year <- start_year
  output_start_month <- start_month
  output_end_year <- end_year
  output_end_month <- end_month
  base_year <- base_year %||% default_hicp_base_year()
  calculation_start_year <- if (is.null(start_year)) {
    NULL
  } else {
    min(start_year, base_year, na.rm = TRUE)
  }
  calculation_start_month <- if (!is.null(start_year) &&
                                 identical(calculation_start_year, start_year)) {
    start_month
  } else {
    1L
  }

  national_indices <- lapply(countries, function(country_i) {
    calculate_price_indices(
      country = country_i,
      category = category,
      level = level,
      start_year = calculation_start_year,
      start_month = calculation_start_month,
      end_year = end_year,
      end_month = end_month,
      ensure_complete_cpi = ensure_complete_cpi,
      interpolated_hbs = interpolated_hbs,
      specific_hbs_year = specific_hbs_year,
      france_insee_income_groups = france_insee_income_groups,
      base_year = base_year,
      include_total = include_total,
      formula = formula,
      recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1,
      aggregate_geo = aggregate_geo,
      weighting_method = weighting_method
    )
  })
  names(national_indices) <- countries

  years <- unique(unlist(lapply(national_indices, function(x) unique(x$dt$year))))
  base_year <- base_year %||% default_hicp_base_year()
  country_weights <- if (is.null(custom_country_weights)) {
    load_country_weights(
      aggregate_geo = aggregate_geo,
      countries = countries,
      start_year = min(years, na.rm = TRUE),
      end_year = max(years, na.rm = TRUE)
    )
  } else {
    normalize_country_weights(custom_country_weights, countries = countries)
  }

  aggregate_indices <- aggregate_price_indices_by_country(
    national_indices,
    country_weights = country_weights,
    aggregate_geo = aggregate_geo,
    category = category,
    level = level,
    base_year = base_year,
    formula = formula
  )

  trim_price_indices(
    aggregate_indices,
    start_year = output_start_year,
    start_month = output_start_month,
    end_year = output_end_year,
    end_month = output_end_month
  )
}

#' Aggregate national price-index objects with country weights
#'
#' @description
#' `aggregate_price_indices_by_country()` combines national `"price_indices"`
#' objects into a country-group aggregate by taking a weighted mean of national
#' unchained monthly movements by date and household group, then chain-linking
#' and rebasing the aggregate. It is used internally when
#' [calculate_price_indices()] is called with several countries.
#'
#' @param price_indices_list named list of `"price_indices"` objects.
#' @param country_weights data frame with columns `country`, `year`, and
#'   `weight` or `country_weight`.
#' @inheritParams calculate_price_indices
#'
#' @returns An object of class `"price_indices"`.
#'
#' @export
aggregate_price_indices_by_country <- function(price_indices_list,
                                               country_weights,
                                               aggregate_geo = "EA20",
                                               category = NULL,
                                               level = NULL,
                                               base_year = NULL,
                                               formula = "laspeyres") {
  if (!is.list(price_indices_list) || length(price_indices_list) == 0) {
    stop("'price_indices_list' must be a non-empty list.")
  }
  if (is.null(names(price_indices_list)) || any(!nzchar(names(price_indices_list)))) {
    stop("'price_indices_list' must be named with country codes.")
  }
  if (!all(vapply(price_indices_list, inherits, logical(1), "price_indices"))) {
    stop("All entries in 'price_indices_list' must be 'price_indices' objects.")
  }

  countries <- toupper(names(price_indices_list))
  weights_dt <- normalize_country_weights(country_weights, countries = countries)

  dt <- data.table::rbindlist(
    lapply(seq_along(price_indices_list), function(i) {
      out <- data.table::copy(price_indices_list[[i]]$dt)
      out[, source_country := countries[[i]]]
      out
    }),
    use.names = TRUE,
    fill = TRUE
  )

  dt <- merge(
    dt,
    weights_dt,
    by.x = c("source_country", "year"),
    by.y = c("country", "year"),
    all.x = TRUE
  )
  if (any(is.na(dt$country_weight))) {
    missing_weights <- unique(dt[is.na(country_weight), .(source_country, year)])
    stop(
      "Missing country weights for: ",
      paste(sprintf("%s-%s", missing_weights$source_country, missing_weights$year),
            collapse = ", ")
    )
  }

  aggregate_dt <- dt[
    ,
    .(
      laspeyres = stats::weighted.mean(laspeyres, country_weight, na.rm = TRUE)
    ),
    by = .(category, year, month, date)
  ]
  data.table::setorder(aggregate_dt, category, date)
  aggregate_dt[, chain_laspeyres := hicp::chain(x = laspeyres, t = date, by = 12),
               by = category]
  aggregate_dt[, price_index := rebase_or_first_available(
    x = chain_laspeyres,
    t = date,
    t.ref = as.character(base_year %||% price_indices_list[[1]]$base_year)
  ), by = category]
  aggregate_dt[, annual_rate := hicp::rates(price_index, t = date, type = "year"),
               by = category]

  categories <- Reduce(
    intersect,
    lapply(price_indices_list, function(x) as.character(x$categories))
  )
  categories <- categories[categories %in% unique(as.character(aggregate_dt$category))]
  if ("Total" %in% unique(as.character(aggregate_dt$category)) &&
      !"Total" %in% categories) {
    categories <- c(categories, "Total")
  }

  structure(
    list(
      dt = aggregate_dt,
      country = aggregate_geo,
      source_countries = countries,
      category = category %||% price_indices_list[[1]]$category,
      categories = categories,
      level = level %||% price_indices_list[[1]]$level,
      start_year = min(aggregate_dt$year),
      start_month = min(aggregate_dt[year == min(year), month]),
      end_year = max(aggregate_dt$year),
      end_month = max(aggregate_dt[year == max(year), month]),
      base_year = base_year %||% price_indices_list[[1]]$base_year,
      formula = formula,
      country_weights = weights_dt
    ),
    class = "price_indices"
  )
}

normalize_country_weights <- function(country_weights, countries = NULL) {
  weights_dt <- data.table::as.data.table(country_weights)
  if (!"country" %in% names(weights_dt)) {
    stop("'country_weights' must contain a 'country' column.")
  }
  if (!"year" %in% names(weights_dt)) {
    stop("'country_weights' must contain a 'year' column.")
  }
  if (!"country_weight" %in% names(weights_dt)) {
    if ("weight" %in% names(weights_dt)) {
      data.table::setnames(weights_dt, "weight", "country_weight")
    } else {
      stop("'country_weights' must contain 'weight' or 'country_weight'.")
    }
  }

  weights_dt <- weights_dt[
    ,
    .(
      country = toupper(as.character(country)),
      year = as.integer(year),
      country_weight = as.numeric(country_weight)
    )
  ]
  weights_dt <- weights_dt[!is.na(country) & !is.na(year) & !is.na(country_weight)]

  if (!is.null(countries)) {
    countries <- toupper(countries)
    weights_dt <- weights_dt[country %in% countries]
    missing_countries <- setdiff(countries, unique(weights_dt$country))
    if (length(missing_countries) > 0) {
      stop("Missing country weights for: ", paste(missing_countries, collapse = ", "))
    }
  }

  weights_dt
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

default_hicp_base_year <- function() {
  2025L
}

trim_price_indices <- function(price_indices_obj,
                               start_year = NULL, start_month = NULL,
                               end_year = NULL, end_month = NULL) {
  dt <- data.table::copy(price_indices_obj$dt)
  if (!is.null(start_year)) {
    dt <- dt[
      year > start_year |
        (year == start_year & month >= (start_month %||% 1L))
    ]
  }
  if (!is.null(end_year)) {
    dt <- dt[
      year < end_year |
        (year == end_year & month <= (end_month %||% 12L))
    ]
  }
  data.table::setorder(dt, category, date)
  price_indices_obj$dt <- dt
  price_indices_obj$start_year <- min(dt$year)
  price_indices_obj$start_month <- min(dt[year == min(year), month])
  price_indices_obj$end_year <- max(dt$year)
  price_indices_obj$end_month <- max(dt[year == max(year), month])
  price_indices_obj
}

rebase_or_first_available <- function(x, t, t.ref) {
  rebased <- suppressWarnings(hicp::rebase(x = x, t = t, t.ref = t.ref))
  if (all(is.na(rebased)) && any(!is.na(x))) {
    first_value <- x[which(!is.na(x))[1]]
    rebased <- x / first_value * 100
  }
  rebased
}

use_france_insee_level3_hbs <- function(country, category, level, custom_hbs) {
  is.null(custom_hbs) &&
    identical(country, "FR") &&
    isTRUE(category %in% c("income", "age", "urban")) &&
    identical(as.numeric(level), 3) &&
    file.exists(france_insee_hbs_level3_path(category))
}

use_euro_area_fast_total <- function(country, custom_hbs) {
  is.null(custom_hbs) && identical(country, "EA")
}

calculate_total_price_index_dt <- function(price_dt, index_weights_obj, base_year,
                                           formula = c("laspeyres", "toernqvist")) {
  formula <- match.arg(formula)
  total_weights <- data.table::copy(index_weights_obj$dt)
  if ("weight_year" %in% names(total_weights)) {
    data.table::setnames(total_weights, "weight_year", "year")
  }
  total_data <- merge(price_dt, total_weights, by = c("coicop", "year"))
  if (formula == "toernqvist") {
    total_weights_previous <- data.table::copy(total_weights)
    total_weights_previous[, year := year + 1L]
    data.table::setnames(total_weights_previous, "weight", "weight_previous")
    total_weights_previous <- total_weights_previous[, .(coicop, year, weight_previous)]
    total_data <- merge(total_data, total_weights_previous, by = c("coicop", "year"), all.x = TRUE)
    total_data[is.na(weight_previous), weight_previous := weight]
    total_dt <- total_data[
      !is.na(dec_ratio) & !is.na(weight) & !is.na(weight_previous),
      .(laspeyres = hicp::toernqvist(x = dec_ratio, w0 = weight_previous, wt = weight)),
      by = .(year, month, date)
    ]
  } else {
    total_dt <- total_data[
      !is.na(dec_ratio) & !is.na(weight),
      .(laspeyres = hicp::laspeyres(x = dec_ratio, w0 = weight)),
      by = .(year, month, date)
    ]
  }
  data.table::setorder(total_dt, date)
  total_dt[, category := "Total"]
  total_dt[, chain_laspeyres := hicp::chain(x = laspeyres, t = date, by = 12)]
  total_dt[, price_index := rebase_or_first_available(
    x = chain_laspeyres,
    t = date,
    t.ref = as.character(base_year)
  )]
  total_dt[, annual_rate := hicp::rates(x = price_index, t = date, type = "year")]
  total_dt[, .(category, year, month, date, laspeyres, chain_laspeyres, price_index, annual_rate)]
}

france_insee_hbs_level3_file_name <- function(category) {
  if (identical(category, "income")) {
    "INSEE_HBS_2017_level3.RDS"
  } else {
    paste0("INSEE_HBS_2017_", category, "_level3.RDS")
  }
}

france_insee_hbs_level3_path <- function(category) {
  file_name <- france_insee_hbs_level3_file_name(category)
  path <- system.file("extdata", file_name, package = "inflationinequality", mustWork = FALSE)

  if (!nzchar(path)) {
    source_path <- file.path("inst", "extdata", file_name)
    vignette_path <- file.path("vignettes", "articles", file_name)
    path <- if (file.exists(source_path)) source_path else vignette_path
  }

  path
}

load_france_insee_hbs_level3 <- function(category = "income",
                                         income_groups = c("decile", "quintile")) {
  if (category %in% c("decile", "quintile")) {
    income_groups <- category
    category <- "income"
  }
  category <- match.arg(category, c("income", "age", "urban"))
  income_groups <- match.arg(income_groups)
  file_name <- france_insee_hbs_level3_file_name(category)
  path <- france_insee_hbs_level3_path(category)

  if (!file.exists(path)) {
    stop(
      "France level 3 ", category, " indices require the bundled INSEE HBS file '",
      file_name,
      "', but it could not be found. Provide 'custom_hbs' explicitly."
    )
  }

  hbs_obj <- readRDS(path)

  if (identical(category, "income") && identical(income_groups, "quintile")) {
    hbs_obj <- aggregate_france_insee_deciles_to_quintiles(hbs_obj)
  }

  hbs_obj
}

aggregate_france_insee_deciles_to_quintiles <- function(hbs_obj) {
  if (length(hbs_obj$categories) != 10) {
    stop("France INSEE decile aggregation expects exactly 10 income groups.")
  }

  dt <- data.table::copy(hbs_obj$dt)
  dt_total <- data.table::copy(hbs_obj$dt_total)
  quintile_categories <- category_data$income$categories
  decile_to_quintile <- data.table::data.table(
    category = hbs_obj$categories,
    quintile = rep(quintile_categories, each = 2)
  )

  dt <- dt[
    decile_to_quintile,
    on = "category",
    nomatch = 0
  ]
  dt <- dt[
    ,
    .(
      series_name = paste(unique(stats::na.omit(series_name)), collapse = "; "),
      consumption = mean(consumption, na.rm = TRUE)
    ),
    by = .(coicop, year, category = quintile)
  ]
  dt[series_name == "", series_name := NA_character_]
  data.table::setcolorder(dt, c("series_name", "coicop", "year", "category", "consumption"))

  hbs(
    dt = dt,
    dt_total = dt_total,
    country = hbs_obj$country,
    category = hbs_obj$category,
    categories = quintile_categories,
    level = hbs_obj$level
  )
}

recode_cpi_ecoicop2_to_ecoicop1 <- function(cpi_obj, target_level, index_weights_obj = NULL) {
  cpi_new <- cpi_obj
  cpi_new$dt <- data.table::copy(cpi_new$dt)
  cpi_new$dt <- cpi_new$dt[nchar(coicop) == cpi_obj$level + 1L]
  cpi_new$dt[, coicop_v2_source := coicop]
  cpi_new$dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(cpi_new$dt, coicop_v2_source, date)
  cpi_new$dt[, dec_ratio := hicp::unchain(x = value, t = date), by = coicop_v2_source]
  cpi_new$dt[, coicop := recode_coicop_ecoicop2_to_ecoicop1(coicop)]
  cpi_new$dt[, coicop := coicop_to_level(coicop, target_level)]

  if (!is.null(index_weights_obj)) {
    cpi_recode_weights <- data.table::copy(index_weights_obj$dt)
    cpi_recode_weight_year_col <- if ("weight_year" %in% names(cpi_recode_weights)) {
      "weight_year"
    } else {
      "year"
    }
    cpi_recode_weights <- cpi_recode_weights[
      nchar(coicop) == index_weights_obj$level + 1L,
      .(coicop_v2_source = coicop, year = get(cpi_recode_weight_year_col), weight)
    ]
    cpi_recode_weights <- cpi_recode_weights[
      ,
      .(weight = sum(weight, na.rm = TRUE)),
      by = .(coicop_v2_source, year)
    ]
    cpi_new$dt <- merge(
      cpi_new$dt,
      cpi_recode_weights,
      by = c("coicop_v2_source", "year"),
      all.x = TRUE
    )
  } else {
    cpi_new$dt[, weight := NA_real_]
  }

  cpi_new$dt <- cpi_new$dt[
    ,
    .(
      series_name = paste(unique(stats::na.omit(series_name)), collapse = "; "),
      dec_ratio = weighted_mean_or_mean(dec_ratio, weight)
    ),
    by = .(coicop, year, month, date)
  ]
  data.table::setorder(cpi_new$dt, coicop, date)
  cpi_new$dt[, value := hicp::chain(x = dec_ratio, t = date, by = 12), by = coicop]
  cpi_new$dt[, c("date", "dec_ratio") := NULL]
  cpi_new$dt[series_name == "", series_name := NA_character_]
  cpi_new$level <- target_level
  cpi_new
}

weighted_mean_or_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & is.finite(w) & w > 0
  if (any(ok)) {
    stats::weighted.mean(x[ok], w[ok], na.rm = TRUE)
  } else {
    mean(x, na.rm = TRUE)
  }
}

recode_index_weights_ecoicop2_to_ecoicop1 <- function(index_weights_obj, target_level) {
  index_weights_new <- index_weights_obj
  index_weights_new$dt <- data.table::copy(index_weights_new$dt)
  weight_year_col <- if ("weight_year" %in% names(index_weights_new$dt)) "weight_year" else "year"
  index_weights_new$dt <- index_weights_new$dt[
    nchar(coicop) == index_weights_obj$level + 1L
  ]
  index_weights_new$dt[, coicop := recode_coicop_ecoicop2_to_ecoicop1(coicop)]
  index_weights_new$dt[, coicop := coicop_to_level(coicop, target_level)]
  index_weights_new$dt <- index_weights_new$dt[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = c("coicop", weight_year_col)
  ]
  index_weights_new$level <- target_level
  index_weights_new$ecoicop2_recoded_to_ecoicop1 <- TRUE
  index_weights_new
}

coicop_to_level <- function(coicop, level) {
  data.table::fifelse(
    coicop == "00",
    coicop,
    substr(coicop, 1, pmin(nchar(coicop), level + 1))
  )
}

recode_coicop_ecoicop2_to_ecoicop1 <- function(coicop) {
  coicop <- as.character(coicop)
  bridge <- ecoicop_v2_to_v1_bridge
  matched <- bridge$coicop_v1[match(coicop, bridge$coicop_v2)]
  data.table::fifelse(is.na(matched), coicop, matched)
}
