#' Calculate upper-level substitution bias
#'
#' @description
#' `calculate_substitution_bias()` compares annualized price-index growth from
#' the package's Eurostat-style chained Laspeyres aggregation with a chained
#' Toernqvist aggregation. The difference is an upper-level substitution-bias
#' measure inspired by the BLS R-CPI-I / R-C-CPI-I comparison, adapted to keep
#' the HICP chained-Laspeyres convention on the reference side.
#'
#' @inheritParams calculate_price_indices
#'
#' @return A `data.table` with one row per group and columns:
#'   `category`, `chained_laspeyres_growth`, `chained_toernqvist_growth`, and
#'   `substitution_bias`. `substitution_bias` is
#'   `chained_laspeyres_growth - chained_toernqvist_growth`.
#'
#' @examples
#' \dontrun{
#' calculate_substitution_bias("FR", "income", level = 2, start_year = 2019)
#' }
#'
#' @export
calculate_substitution_bias <- function(country = NULL, category = NULL, level = 2,
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
                                        recode_ecoicop2_to_ecoicop1 = TRUE) {
  france_insee_income_groups <- match.arg(france_insee_income_groups)

  chained_laspeyres <- calculate_price_indices(
    country = country,
    category = category,
    level = level,
    start_year = start_year,
    start_month = start_month,
    end_year = end_year,
    end_month = end_month,
    ensure_complete_cpi = ensure_complete_cpi,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    interpolated_hbs = interpolated_hbs,
    specific_hbs_year = specific_hbs_year,
    france_insee_income_groups = france_insee_income_groups,
    base_year = base_year,
    include_total = include_total,
    formula = "laspeyres",
    recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
  )
  chained_toernqvist <- calculate_price_indices(
    country = country,
    category = category,
    level = level,
    start_year = start_year,
    start_month = start_month,
    end_year = end_year,
    end_month = end_month,
    ensure_complete_cpi = ensure_complete_cpi,
    custom_cpi = custom_cpi,
    custom_index_weights = custom_index_weights,
    custom_hbs = custom_hbs,
    interpolated_hbs = interpolated_hbs,
    specific_hbs_year = specific_hbs_year,
    france_insee_income_groups = france_insee_income_groups,
    base_year = base_year,
    include_total = include_total,
    formula = "toernqvist",
    recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
  )

  chained_laspeyres_growth <- annualized_price_index_growth(chained_laspeyres$dt)
  data.table::setnames(
    chained_laspeyres_growth,
    "growth",
    "chained_laspeyres_growth"
  )
  chained_toernqvist_growth <- annualized_price_index_growth(chained_toernqvist$dt)
  data.table::setnames(
    chained_toernqvist_growth,
    "growth",
    "chained_toernqvist_growth"
  )

  out <- merge(chained_laspeyres_growth, chained_toernqvist_growth, by = "category")
  out[, substitution_bias := chained_laspeyres_growth - chained_toernqvist_growth]

  category_levels <- if (include_total) {
    c("Total", chained_laspeyres$categories)
  } else {
    chained_laspeyres$categories
  }
  out[, category := factor(category, levels = unique(category_levels))]
  data.table::setorder(out, category)
  out[]
}

annualized_price_index_growth <- function(dt) {
  dt <- data.table::copy(dt)
  data.table::setorder(dt, category, date)
  dt[
    ,
    {
      first_row <- .SD[which(!is.na(price_index))[1]]
      last_row <- .SD[rev(which(!is.na(price_index)))[1]]
      month_diff <- (last_row$year - first_row$year) * 12 +
        (last_row$month - first_row$month)
      growth <- if (month_diff > 0) {
        ((last_row$price_index / first_row$price_index)^(12 / month_diff) - 1) * 100
      } else {
        NA_real_
      }
      .(growth = growth)
    },
    by = category
  ]
}
