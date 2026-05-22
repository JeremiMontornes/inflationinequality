#' Compare calculated average HICP with published HICP
#'
#' @description
#' `compare_to_official_hicp()` validates the average HICP calculated from
#' household-category results against the published all-items HICP.
#'
#' @param inflation An object of class `"inflation"` or `"price_indices"`.
#' @param country Optional 2-digit country code. If `NULL`, the country stored
#'   in `inflation` is used.
#' @param custom_cpi Optional object of class `"cpi"` containing the official
#'   all-items basket in `dt_basket`. If `NULL`, the function downloads HICP
#'   data with [load_cpi()].
#' @param measure validation measure. If `NULL`, `"rate"` is used for
#'   `"inflation"` objects and `"level"` for `"price_indices"` objects.
#'
#' @returns An object of class `"hicp_comparison"` containing:
#' - `dt`: date-by-date comparison between calculated and published HICP.
#' - `summary`: validation statistics.
#' - `plot`: a ggplot object with grey bars for the date-by-date difference.
#' - `country`: country code.
#' - `measure`: comparison measure (`"rate"` or `"level"`).
#'
#' @details
#' With an `"inflation"` object, the calculated average is obtained with
#' [calculate_total_inflation()], ie the mean across household categories, and
#' compared with the year-on-year rate of the published all-items HICP.
#'
#' With a `"price_indices"` object, the calculated average is the mean of
#' household-category price indices. A `"Total"` category, if present, is
#' excluded from the average. The published all-items HICP is rebased to the
#' same base year before comparison.
#'
#' @examples
#' \dontrun{
#' inflation <- calculate_inflation("FR", "income", start_year = 2019)
#' comparison <- compare_to_official_hicp(inflation)
#' comparison$plot
#' comparison$summary
#'
#' indices <- calculate_price_indices("FR", "income", start_year = 2019, base_year = 2010)
#' level_comparison <- compare_to_official_hicp(indices)
#' level_comparison$plot
#' }
#'
#' @export
compare_to_official_hicp <- function(inflation, country = NULL, custom_cpi = NULL,
                                     measure = NULL) {
  if (!inherits(inflation, "inflation") && !inherits(inflation, "price_indices")) {
    stop("'inflation' must be an object of class 'inflation' or 'price_indices'.")
  }

  if (is.null(measure)) {
    measure <- if (inherits(inflation, "price_indices")) "level" else "rate"
  }
  measure <- match.arg(measure, c("rate", "level"))

  if (measure == "level" && !inherits(inflation, "price_indices")) {
    stop("measure = 'level' requires an object of class 'price_indices'.")
  }

  country <- country %||% inflation$country
  if (is.null(country) && is.null(custom_cpi)) {
    stop("Either 'country' or 'custom_cpi' must be provided.")
  }

  if (measure == "rate") {
    calculated_dt <- calculate_total_inflation(inflation)
    calculated_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
    data.table::setnames(calculated_dt, "total_inflation", "calculated_value")
    calculated_min_year <- min(calculated_dt$year, na.rm = TRUE) - 1
    y_label <- "Year-on-year change / difference (pp)"
    plot_title <- "Calculated average inflation vs published HICP"
  } else {
    calculated_dt <- data.table::copy(inflation$dt)
    calculated_dt <- calculated_dt[category != "Total"]
    calculated_dt <- calculated_dt[, .(calculated_value = mean(price_index, na.rm = TRUE)),
                                   by = .(year, month, date)]
    comparison_base_year <- inflation$base_year %||% min(calculated_dt$year, na.rm = TRUE)
    if (comparison_base_year < min(calculated_dt$year, na.rm = TRUE)) {
      warning(
        sprintf(
          "base_year=%s is before the first calculated price-index year (%s). ",
          comparison_base_year,
          min(calculated_dt$year, na.rm = TRUE)
        ),
        "The published HICP is rebased to the first calculated year for comparability."
      )
      comparison_base_year <- min(calculated_dt$year, na.rm = TRUE)
    }
    calculated_min_year <- comparison_base_year
    y_label <- "Index level / difference"
    plot_title <- "Calculated average price index vs published HICP"
  }

  cpi_obj <- if (is.null(custom_cpi)) {
    load_cpi(
      country,
      level = inflation$level %||% 2,
      start_year = calculated_min_year,
      end_year = max(calculated_dt$year, na.rm = TRUE),
      end_month = max(calculated_dt[year == max(year), month], na.rm = TRUE)
    )
  } else {
    custom_cpi
  }

  if (!inherits(cpi_obj, "cpi")) {
    stop("'custom_cpi' must be an object of class 'cpi'.")
  }

  official_dt <- data.table::copy(cpi_obj$dt_basket)
  official_dt[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  data.table::setorder(official_dt, date)
  if (measure == "rate") {
    official_dt[, official_value := hicp::rates(value, t = date, type = "year")]
  } else {
    official_dt[, official_value := rebase_or_first_available(
      x = value,
      t = date,
      t.ref = as.character(comparison_base_year)
    )]
  }
  official_dt <- official_dt[, .(year, month, date, official_value)]

  comparison_dt <- merge(
    calculated_dt[, .(year, month, date, calculated_value)],
    official_dt,
    by = c("year", "month", "date"),
    all.x = TRUE
  )
  comparison_dt[, difference := calculated_value - official_value]

  summary_dt <- comparison_dt[
    !is.na(calculated_value) & !is.na(official_value),
    .(
      n = .N,
      mean_difference = mean(difference, na.rm = TRUE),
      mean_abs_difference = mean(abs(difference), na.rm = TRUE),
      rmse = sqrt(mean(difference^2, na.rm = TRUE)),
      max_abs_difference = max(abs(difference), na.rm = TRUE)
    )
  ]

  line_dt <- data.table::melt(
    comparison_dt,
    id.vars = c("year", "month", "date", "difference"),
    measure.vars = c("calculated_value", "official_value"),
    variable.name = "series",
    value.name = "value"
  )
  line_dt[, series := data.table::fifelse(
    series == "calculated_value",
    "Calculated average",
    "Published HICP"
  )]

  p <- ggplot2::ggplot(comparison_dt, ggplot2::aes(x = date)) +
    ggplot2::geom_col(
      ggplot2::aes(y = difference),
      fill = "grey75",
      color = "grey75",
      width = 25,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
    ggplot2::geom_line(
      data = line_dt,
      ggplot2::aes(y = value, color = series),
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    ggplot2::scale_color_manual(
      values = c("Calculated average" = "black", "Published HICP" = "#2f6fbb")
    ) +
    ggplot2::labs(
      x = "",
      y = y_label,
      color = "",
      title = plot_title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12)
    )

  if (measure == "level") {
    p <- p + ggplot2::coord_cartesian(ylim = c(95, NA))
  }

  structure(
    list(
      dt = comparison_dt,
      summary = summary_dt,
      plot = p,
      country = country %||% cpi_obj$country,
      measure = measure
    ),
    class = "hicp_comparison"
  )
}
