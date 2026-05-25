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
#'   the first returned year is used.
#' @param include_total whether to include the official all-items HICP index as
#'   a `"Total"` category.
#' @param formula upper-level index formula. `"laspeyres"` uses the package's
#'   current Lowe-style fixed-quantity aggregation with annual weights.
#'   `"toernqvist"` uses [hicp::toernqvist()] with previous-year and current-year
#'   category weights as an approximation to a chained Törnqvist index.
#' @param recode_ecoicop2_to_ecoicop1 whether to map ECOICOP v2 HICP item
#'   codes back to ECOICOP v1-style codes before matching them to HBS weights.
#'
#' @section France level 3:
#' For France income groups, `level = 3` automatically uses the bundled INSEE
#' 2017 HBS level-3 data when `custom_hbs` is not supplied. Eurostat public HBS
#' data are generally not available at this granularity, so the national HBS
#' source is required for a genuine 4-digit COICOP calculation.
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
                                    recode_ecoicop2_to_ecoicop1 = TRUE) {
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
  france_insee_income_groups <- match.arg(france_insee_income_groups)

  if (use_france_insee_level3_hbs(country, category, level, custom_hbs)) {
    custom_hbs <- load_france_insee_hbs_level3(
      income_groups = france_insee_income_groups
    )
  }

  data_start_year <- if (!is.null(start_year)) start_year - 1 else NULL
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
  base_year <- if (is.null(base_year)) start_year_out else base_year

  index_weights_obj <- if (is.null(custom_index_weights)) {
    load_index_weights(
      country %||% cpi_obj$country,
      level = hicp_level,
      start_year = start_year_out,
      end_year = end_year_out
    )
  } else {
    custom_index_weights
  }

  if (recode_ecoicop2_to_ecoicop1) {
    cpi_obj <- recode_cpi_ecoicop2_to_ecoicop1(cpi_obj, target_level = level)
    index_weights_obj <- recode_index_weights_ecoicop2_to_ecoicop1(index_weights_obj, target_level = level)
  }

  weights <- calculate_weights(
    country = country %||% cpi_obj$country,
    category = category %||% custom_hbs$category,
    level = level,
    start_year = if (is.null(custom_index_weights) && is.null(custom_hbs)) start_year_out else NULL,
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
    france_insee_income_groups = france_insee_income_groups
  )

  price_dt <- data.table::copy(cpi_obj$dt)
  price_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(price_dt, coicop, date)
  price_dt[, dec_ratio := hicp::unchain(x = value, t = date), by = coicop]

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
      formula = formula
    ),
    class = "price_indices"
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
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
    identical(category, "income") &&
    identical(as.numeric(level), 3)
}

load_france_insee_hbs_level3 <- function(income_groups = c("decile", "quintile")) {
  income_groups <- match.arg(income_groups)
  file_name <- "INSEE_HBS_2017_level3.RDS"
  path <- system.file("extdata", file_name, package = "inflationinequality", mustWork = FALSE)

  if (!nzchar(path)) {
    source_path <- file.path("inst", "extdata", file_name)
    vignette_path <- file.path("vignettes", "articles", file_name)
    path <- if (file.exists(source_path)) source_path else vignette_path
  }

  if (!file.exists(path)) {
    stop(
      "France level 3 income indices require the bundled INSEE HBS file '",
      file_name,
      "', but it could not be found. Provide 'custom_hbs' explicitly."
    )
  }

  hbs_obj <- readRDS(path)

  if (identical(income_groups, "quintile")) {
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

recode_cpi_ecoicop2_to_ecoicop1 <- function(cpi_obj, target_level) {
  cpi_new <- cpi_obj
  cpi_new$dt <- data.table::copy(cpi_new$dt)
  cpi_new$dt[, coicop := recode_coicop_ecoicop2_to_ecoicop1(coicop)]
  cpi_new$dt[, coicop := coicop_to_level(coicop, target_level)]
  cpi_new$dt <- cpi_new$dt[
    ,
    .(
      series_name = paste(unique(stats::na.omit(series_name)), collapse = "; "),
      value = mean(value, na.rm = TRUE)
    ),
    by = .(coicop, year, month)
  ]
  cpi_new$dt[series_name == "", series_name := NA_character_]
  cpi_new$level <- target_level
  cpi_new
}

recode_index_weights_ecoicop2_to_ecoicop1 <- function(index_weights_obj, target_level) {
  index_weights_new <- index_weights_obj
  index_weights_new$dt <- data.table::copy(index_weights_new$dt)
  weight_year_col <- if ("weight_year" %in% names(index_weights_new$dt)) "weight_year" else "year"
  index_weights_new$dt[, coicop := recode_coicop_ecoicop2_to_ecoicop1(coicop)]
  index_weights_new$dt[, coicop := coicop_to_level(coicop, target_level)]
  index_weights_new$dt <- index_weights_new$dt[
    ,
    .(weight = sum(weight, na.rm = TRUE)),
    by = c("coicop", weight_year_col)
  ]
  index_weights_new$level <- target_level
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
  dplyr::case_when(
    coicop %in% c("00") ~ "00",
    coicop %in% c("0122", "0123", "0124") ~ "0121",
    coicop %in% c("0121", "0125", "0126", "0129", "0130") ~ "0122",
    coicop %in% c("0220", "0219", "0230") ~ "022",
    coicop %in% c("0521") ~ "0513",
    coicop %in% c("0522") ~ "052",
    coicop %in% c("0540") ~ "054",
    coicop %in% c("0553") ~ "0552",
    coicop %in% c("0614") ~ "0613",
    coicop %in% c("0631", "0632") ~ "063",
    coicop %in% c("0641", "0642") ~ "063",
    coicop %in% c("0741", "0749") ~ "081",
    coicop %in% c("0811", "0812", "0819") ~ "0820",
    coicop %in% c("0814") ~ "0911",
    coicop %in% c("0813") ~ "0913",
    coicop %in% c("0815") ~ "0914",
    coicop %in% c("0841") ~ "0915",
    coicop %in% c("0821", "0822", "0823", "0824", "0825", "0829") ~ "082",
    coicop %in% c("0831", "0832", "0833", "0834", "0835", "0839") ~ "0830",
    coicop %in% c("0911") ~ "0912",
    coicop %in% c("0941") ~ "0915",
    coicop %in% c("0912") ~ "0921",
    coicop %in% c("0951") ~ "0922",
    coicop %in% c("0942") ~ "0923",
    coicop %in% c("0921") ~ "0931",
    coicop %in% c("0922") ~ "0932",
    coicop %in% c("0931", "0943") ~ "0933",
    coicop %in% c("0932") ~ "0934",
    coicop %in% c("0945") ~ "0935",
    coicop %in% c("0946", "0944", "0947") ~ "0941",
    coicop %in% c("0961", "0962", "0963", "0969") ~ "0942",
    coicop %in% c("0971") ~ "0951",
    coicop %in% c("0972") ~ "0952",
    coicop %in% c("0973") ~ "0953",
    coicop %in% c("0974") ~ "0954",
    coicop %in% c("0971", "0972", "0973", "0974", "0980") ~ "096",
    coicop %in% c("1010") ~ "101",
    coicop %in% c("1020") ~ "102",
    coicop %in% c("1030") ~ "103",
    coicop %in% c("1040") ~ "104",
    coicop %in% c("1050") ~ "105",
    coicop %in% c("1120") ~ "112",
    coicop %in% c("1311") ~ "1212",
    coicop %in% c("1312") ~ "1213",
    coicop %in% c("1321") ~ "1231",
    coicop %in% c("1322", "1329") ~ "1232",
    coicop %in% c("1213") ~ "1252",
    coicop %in% c("1212") ~ "1253",
    coicop %in% c("1214") ~ "1254",
    coicop %in% c("1219") ~ "1255",
    coicop %in% c("1330") ~ "124",
    coicop %in% c("1222", "1312", "1229") ~ "126",
    coicop %in% c("1390", "1313") ~ "127",
    TRUE ~ coicop
  )
}
