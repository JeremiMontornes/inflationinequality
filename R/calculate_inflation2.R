#' Calculate year-on-year inflation from monthly price indices
#'
#' @description
#' `calculate_inflation2()` computes monthly year-on-year inflation rates
#' directly from the category-specific price indices returned by
#' [calculate_price_indices()]. For each category and month, inflation is
#' defined as
#'
#' `100 * (price_index[t] / price_index[t - 12] - 1)`.
#'
#' This makes the reported inflation rate algebraically consistent with the
#' corresponding monthly price index. Months without an observation twelve
#' months earlier are omitted.
#'
#' @inheritParams calculate_price_indices
#'
#' @returns An object of class `"inflation"` containing:
#' - `dt`: a `data.table` with columns `year`, `month`, `category`, and
#'   `inflation`.
#' - `country`, `category`, `categories`, `weighting_method`, `level`,
#'   `start_year`, `start_month`, `end_year`, and `end_month`: calculation
#'   metadata.
#' - `price_indices`: the underlying `"price_indices"` object used to compute
#'   the rates.
#'
#' @examples
#' \dontrun{
#' france_inflation <- calculate_inflation2(
#'   "FR", "income", start_year = 2019, end_year = 2024
#' )
#' france_inflation$dt
#' }
#'
#' @seealso [calculate_inflation()], [calculate_price_indices()]
#'
#' @export
calculate_inflation2 <- function(country = NULL, category = NULL, level = 2,
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
                                 include_total = FALSE,
                                 formula = c("laspeyres", "toernqvist"),
                                 recode_ecoicop2_to_ecoicop1 = TRUE,
                                 aggregate_geo = "EA20",
                                 custom_country_weights = NULL,
                                 weighting_method = c("relative_expenditure", "ras")) {
  formula <- match.arg(formula)
  weighting_method <- match.arg(weighting_method)
  france_insee_income_groups <- match.arg(france_insee_income_groups)

  indices <- calculate_price_indices(
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
    formula = formula,
    recode_ecoicop2_to_ecoicop1 = recode_ecoicop2_to_ecoicop1,
    aggregate_geo = aggregate_geo,
    custom_country_weights = custom_country_weights,
    weighting_method = weighting_method
  )

  dt_inflation <- data.table::copy(indices$dt)[
    !is.na(annual_rate),
    .(year, month, category, inflation = annual_rate)
  ]

  if (nrow(dt_inflation) == 0L) {
    first_year <- first_month <- last_year <- last_month <- NA_integer_
  } else {
    first_year <- min(dt_inflation$year)
    first_month <- min(dt_inflation[year == first_year, month])
    last_year <- max(dt_inflation$year)
    last_month <- max(dt_inflation[year == last_year, month])
  }

  structure(
    list(
      dt = dt_inflation,
      dt_missing_weight = indices$dt_missing_weight %||% data.table::data.table(),
      dt_coverage = indices$dt_coverage %||% data.table::data.table(),
      country = indices$country,
      source_countries = indices$source_countries %||% NULL,
      category = indices$category,
      categories = indices$categories,
      weighting_method = weighting_method,
      formula = formula,
      level = indices$level,
      start_year = first_year,
      start_month = first_month,
      end_year = last_year,
      end_month = last_month,
      price_indices = indices
    ),
    class = "inflation"
  )
}
