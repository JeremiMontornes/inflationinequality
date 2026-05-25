#' Calculate aggregation bias in the inflation gap
#'
#' @description
#' `calculate_aggregation_bias()` compares the annualized inflation gap between
#' the lowest and highest household groups at two COICOP aggregation levels. The
#' gap is defined as bottom-group annualized price-index growth minus top-group
#' annualized price-index growth. Aggregation bias is defined as the gap at the
#' upper COICOP level, meaning the more aggregated calculation, minus the gap at
#' the lower COICOP level, meaning the more detailed calculation.
#'
#' @inheritParams calculate_price_indices
#' @param coarse_level COICOP level used for the coarser calculation. If
#'   `NULL`, it is set to `fine_level - 1`.
#' @param fine_level COICOP level used for the finer calculation. If `NULL`, the
#'   function uses level 3 when an integrated country-specific level 3 source is
#'   available, and level 2 otherwise.
#'
#' @return A one-row `data.table` with columns:
#'   `country`, `category_type`, `coarse_level`, `fine_level`,
#'   `coarse_gap`, `fine_gap`, and `aggregation_bias`.
#'
#' @examples
#' \dontrun{
#' calculate_aggregation_bias("DE", "income", coarse_level = 1, fine_level = 2,
#'                            start_year = 2019)
#' }
#'
#' @export
calculate_aggregation_bias <- function(country = NULL, category = NULL,
                                       coarse_level = NULL,
                                       fine_level = NULL,
                                       start_year = NULL,
                                       start_month = NULL,
                                       end_year = NULL,
                                       end_month = NULL,
                                       ensure_complete_cpi = FALSE,
                                       custom_cpi = NULL,
                                       custom_index_weights = NULL,
                                       custom_hbs = NULL,
                                       interpolated_hbs = FALSE,
                                       specific_hbs_year = NULL,
                                       france_insee_income_groups = c("decile", "quintile"),
                                       base_year = NULL,
                                       recode_ecoicop2_to_ecoicop1 = TRUE) {
  france_insee_income_groups <- match.arg(france_insee_income_groups)

  if (is.null(coarse_level)) {
    if (is.null(fine_level)) {
      fine_level <- default_fine_aggregation_level(country, category)
    }
    coarse_level <- fine_level - 1
  }
  if (is.null(fine_level)) {
    fine_level <- coarse_level + 1
  }
  if (coarse_level >= fine_level) {
    stop("'coarse_level' must be lower than 'fine_level'.")
  }

  coarse_indices <- calculate_price_indices(
    country = country,
    category = category,
    level = coarse_level,
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
    include_total = FALSE,
    formula = "laspeyres",
    recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
  )

  fine_indices <- calculate_price_indices(
    country = country,
    category = category,
    level = fine_level,
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
    include_total = FALSE,
    formula = "laspeyres",
    recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1
  )

  coarse_gap <- annualized_group_gap(coarse_indices$dt, coarse_indices$categories)
  fine_gap <- annualized_group_gap(fine_indices$dt, fine_indices$categories)

  data.table::data.table(
    country = country,
    category_type = category,
    coarse_level = coarse_level,
    fine_level = fine_level,
    coarse_gap = coarse_gap,
    fine_gap = fine_gap,
    aggregation_bias = coarse_gap - fine_gap
  )
}

default_fine_aggregation_level <- function(country, category) {
  if (!is.null(country) && !is.null(category) &&
      toupper(country) == "FR" && category == "income") {
    return(3)
  }
  2
}

annualized_group_gap <- function(dt, categories) {
  growth <- annualized_price_index_growth(dt)
  bottom <- categories[1]
  top <- categories[length(categories)]

  bottom_growth <- growth[category == bottom, growth]
  top_growth <- growth[category == top, growth]
  if (length(bottom_growth) != 1 || length(top_growth) != 1) {
    stop("Could not identify bottom and top category growth rates.")
  }

  bottom_growth - top_growth
}
