#' Load aggregate propensity to consume by income quintile
#'
#' @description
#' `load_consumption_to_income()` downloads Eurostat experimental statistics
#' on aggregate propensity to consume by income quintile (`icw_sr_10`). Values
#' are percentages of disposable income.
#'
#' @param country 2-digit country code.
#' @param start_year Optional first year to keep.
#' @param end_year Optional last year to keep.
#'
#' @return A `data.table` with columns `category`, `year`, and
#'   `consumption_to_income`.
#'
#' @examples
#' \dontrun{
#' load_consumption_to_income("FR")
#' }
#'
#' @export
load_consumption_to_income <- function(country, start_year = NULL, end_year = NULL) {
  load_income_quintile_indicator(
    country = country,
    dataset = "icw_sr_10",
    value_col = "consumption_to_income",
    unit = "PC_DI",
    start_year = start_year,
    end_year = end_year
  )
}

#' Load mean consumption expenditure by income quintile
#'
#' @description
#' `load_consumption_expenditure()` downloads Eurostat HBS mean consumption
#' expenditure by income quintile (`hbs_exp_t133`). Values are available in PPS
#' per household (`"PPS_HH"`) or PPS per adult equivalent (`"PPS_AE"`).
#'
#' @inheritParams load_consumption_to_income
#' @param unit Eurostat unit to use. Defaults to `"PPS_HH"`.
#'
#' @return A `data.table` with columns `category`, `year`, and `expenditure`.
#'
#' @examples
#' \dontrun{
#' load_consumption_expenditure("FR", unit = "PPS_HH")
#' }
#'
#' @export
load_consumption_expenditure <- function(country, start_year = NULL,
                                         end_year = NULL, unit = "PPS_HH") {
  load_income_quintile_indicator(
    country = country,
    dataset = "hbs_exp_t133",
    value_col = "expenditure",
    unit = unit,
    start_year = start_year,
    end_year = end_year
  )
}

#' Calculate inflation burden by household group
#'
#' @description
#' `calculate_inflation_burden()` combines group-specific inflation rates with
#' either consumption-to-income ratios, mean consumption expenditure, or both.
#' The default Eurostat path is available for income quintiles.
#'
#' @details
#' If `consumption_to_income` is available, `inflation_burden` is expressed as a
#' percentage of disposable income:
#'
#' `inflation_burden = inflation * consumption_to_income / 100`
#'
#' If `expenditure` is available, `inflation_cost` is expressed in the same unit
#' as expenditure:
#'
#' `inflation_cost = expenditure * inflation / 100`
#'
#' Annual Eurostat values are matched to monthly inflation with the latest
#' available year not later than the inflation year.
#'
#' @param inflation An object of class `"inflation"` returned by
#'   [calculate_inflation()].
#' @param consumption_to_income Optional `data.table` with columns `category`,
#'   `year`, and `consumption_to_income`, where `consumption_to_income` is a
#'   percentage of disposable income. If `NULL` and `inflation$category` is
#'   `"income"`, Eurostat `icw_sr_10` is loaded.
#' @param expenditure Optional `data.table` with columns `category`, `year`, and
#'   `expenditure`. If `NULL` and `include_expenditure = TRUE`,
#'   Eurostat `hbs_exp_t133` is loaded.
#' @param include_expenditure Whether to include `expenditure` and
#'   `inflation_cost` in the result.
#' @param expenditure_unit Eurostat unit used when loading expenditure.
#'
#' @return An object of class `"inflation_burden"` containing a `data.table`
#'   with monthly inflation, consumption-to-income ratios, inflation burden,
#'   and optionally expenditure and inflation cost.
#'
#' @examples
#' \dontrun{
#' inflation <- calculate_inflation("FR", "income", level = 2, start_year = 2019)
#' burden <- calculate_inflation_burden(inflation)
#' plot_inflation_burden(burden)
#' }
#'
#' @export
calculate_inflation_burden <- function(inflation,
                                       consumption_to_income = NULL,
                                       expenditure = NULL,
                                       include_expenditure = TRUE,
                                       expenditure_unit = "PPS_HH") {
  if (!inherits(inflation, "inflation")) {
    stop("'inflation' must be an object returned by calculate_inflation().")
  }
  if (!is.logical(include_expenditure) || length(include_expenditure) != 1 ||
      is.na(include_expenditure)) {
    stop("'include_expenditure' must be TRUE or FALSE.")
  }

  if (is.null(consumption_to_income)) {
    if (!identical(inflation$category, "income")) {
      stop(
        "'consumption_to_income' must be supplied for non-income inflation objects."
      )
    }
    consumption_to_income <- load_consumption_to_income(inflation$country)
  }

  if (include_expenditure && is.null(expenditure)) {
    if (!identical(inflation$category, "income")) {
      stop("'expenditure' must be supplied for non-income inflation objects.")
    }
    expenditure <- load_consumption_expenditure(
      inflation$country,
      unit = expenditure_unit
    )
  }

  dt <- data.table::copy(inflation$dt)
  dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(dt, category, year, month)

  consumption_dt <- standardize_burden_input(
    consumption_to_income,
    value_col = "consumption_to_income"
  )
  dt <- join_latest_year(dt, consumption_dt, "consumption_to_income")
  dt[, inflation_burden := inflation * consumption_to_income / 100]

  if (include_expenditure) {
    expenditure_dt <- standardize_burden_input(expenditure, value_col = "expenditure")
    dt <- join_latest_year(dt, expenditure_dt, "expenditure")
    dt[, inflation_cost := expenditure * inflation / 100]
  }

  categories <- intersect(inflation$categories, unique(as.character(dt$category)))
  dt[, category := factor(category, levels = categories)]

  structure(
    list(
      dt = dt,
      country = inflation$country,
      category = inflation$category,
      categories = categories,
      level = inflation$level,
      expenditure_unit = if (include_expenditure) expenditure_unit else NULL
    ),
    class = "inflation_burden"
  )
}

load_income_quintile_indicator <- function(country, dataset, value_col, unit,
                                           start_year = NULL, end_year = NULL) {
  if (!is.character(country) || length(country) != 1 || nchar(country) != 2) {
    stop("'country' must be a 2-character ISO code.")
  }

  country <- toupper(country)
  dt <- rdbnomics::rdb(
    "Eurostat",
    dataset,
    dimensions = list(freq = "A", geo = country, unit = unit)
  )
  dt <- data.table::as.data.table(dt)
  if (nrow(dt) == 0) {
    stop(sprintf("No Eurostat data found in '%s' for %s.", dataset, country))
  }

  dt <- dt[
    quant_inc %in% c(category_data$income$old_names, "TOTAL"),
    .(
      category = data.table::fifelse(
        quant_inc == "TOTAL",
        "Total",
        category_data$income$categories[match(quant_inc, category_data$income$old_names)]
      ),
      year = lubridate::year(period),
      value = value
    )
  ]

  if (!is.null(start_year)) {
    dt <- dt[year >= start_year]
  }
  if (!is.null(end_year)) {
    dt <- dt[year <= end_year]
  }
  data.table::setnames(dt, "value", value_col)
  data.table::setorder(dt, category, year)
  dt
}

standardize_burden_input <- function(dt, value_col) {
  dt <- data.table::as.data.table(dt)
  required <- c("category", "year", value_col)
  missing_cols <- setdiff(required, names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "'%s' must contain columns: %s.",
      value_col,
      paste(required, collapse = ", ")
    ))
  }

  dt <- data.table::copy(dt[, ..required])
  dt[, category := as.character(category)]
  dt[, year := as.integer(year)]
  dt[, (value_col) := as.numeric(get(value_col))]
  dt <- dt[!is.na(category) & !is.na(year) & !is.na(get(value_col))]
  data.table::setorder(dt, category, year)
  dt
}

join_latest_year <- function(monthly_dt, annual_dt, value_col) {
  x <- data.table::copy(monthly_dt)
  annual <- data.table::copy(annual_dt)
  source_year_col <- paste0(value_col, "_year")
  annual[, (source_year_col) := year]
  data.table::setkey(annual, category, year)
  data.table::setkey(x, category, year)

  joined <- annual[x, roll = Inf]
  missing_rows <- joined[is.na(get(value_col)), unique(as.character(category))]
  if (length(missing_rows) > 0) {
    stop(sprintf(
      "No '%s' data available for categories: %s.",
      value_col,
      paste(missing_rows, collapse = ", ")
    ))
  }

  joined
}
