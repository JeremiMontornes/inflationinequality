#' Plot the distribution of household-level inflation
#'
#' @description
#' `plot_household_inflation_distribution()` plots the cross-household
#' distribution of annual mean inflation rates returned by
#' [calculate_household_inflation()] or by a pre-aggregated household-year table.
#'
#' @param x A `"household_inflation"` object, or a data frame/data.table with
#'   columns `household_id`, `year`, `mean_inflation`, and optionally `weight`.
#'   If `x` is a `"household_inflation"` object, annual means are computed from
#'   `x$dt$inflation`.
#' @param years Optional years to display.
#' @param bin_width Histogram bin width in percentage points. Defaults to `0.1`.
#' @param overlay If `TRUE`, plot all years on a single graph. If `FALSE`, use
#'   facets, one panel per year.
#' @param weighted If `TRUE`, use the household survey weight column when
#'   available.
#' @param xlim Optional numeric vector of length 2 to zoom the x-axis.
#' @param title,subtitle,xlab,ylab Plot labels.
#'
#' @returns A `ggplot2` object.
#'
#' @examples
#' \dontrun{
#' hh <- calculate_household_inflation(years = 2020, start_year = 2021)
#' plot_household_inflation_distribution(hh, years = 2021:2024)
#' }
#'
#' @export
plot_household_inflation_distribution <- function(x,
                                                  years = NULL,
                                                  bin_width = 0.1,
                                                  overlay = TRUE,
                                                  weighted = TRUE,
                                                  xlim = NULL,
                                                  title = "Distribution of household-level inflation in Spain",
                                                  subtitle = NULL,
                                                  xlab = "Mean annual inflation by household",
                                                  ylab = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to plot household inflation distributions.", call. = FALSE)
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required to plot household inflation distributions.", call. = FALSE)
  }

  annual_mean <- household_inflation_annual_mean_dt(x)
  if (!is.null(years)) {
    annual_mean <- annual_mean[year %in% years]
  }
  if (nrow(annual_mean) == 0L) {
    stop("No household inflation observations to plot.", call. = FALSE)
  }
  if (!"weight" %in% names(annual_mean) || !isTRUE(weighted)) {
    annual_mean[, weight := 1]
  }

  hist_dt <- household_inflation_histogram_dt(annual_mean, bin_width = bin_width)
  hist_dt[, year_label := factor(year, levels = sort(unique(year)))]
  legend_title <- "Annee"
  ylab <- ylab %||% if (isTRUE(weighted)) {
    "Part ponderee des menages"
  } else {
    "Part des menages"
  }
  subtitle <- subtitle %||% sprintf(
    "Paniers menages EPF; inflation moyenne annuelle, classes de %.1f point",
    bin_width
  )

  p <- ggplot2::ggplot(
    hist_dt,
    ggplot2::aes(
      x = bin_mid,
      y = share_households,
      color = year_label,
      fill = year_label
    )
  )

  if (isTRUE(overlay)) {
    p <- p +
      ggplot2::geom_col(
        width = bin_width,
        alpha = 0.22,
        position = "identity",
        linewidth = 0
      ) +
      ggplot2::geom_step(
        ggplot2::aes(x = inflation_bin, y = share_households),
        linewidth = 0.8,
        direction = "mid"
      )
  } else {
    p <- p +
      ggplot2::geom_col(width = bin_width, linewidth = 0, show.legend = FALSE) +
      ggplot2::facet_wrap(ggplot2::vars(year_label), ncol = 2, scales = "free_y")
  }

  p <- p +
    ggplot2::scale_x_continuous(labels = scales::label_number(suffix = "%", accuracy = 1)) +
    ggplot2::scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 0.1)) +
    ggplot2::scale_color_brewer(palette = "Dark2", name = legend_title) +
    ggplot2::scale_fill_brewer(palette = "Dark2", name = legend_title) +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold")
    )

  if (!is.null(xlim)) {
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }

  p
}

household_inflation_annual_mean_dt <- function(x) {
  if (inherits(x, "household_inflation")) {
    dt <- data.table::copy(x$dt)
    if (!all(c("household_id", "year", "inflation") %in% names(dt))) {
      stop("'household_inflation' object does not contain the expected columns.", call. = FALSE)
    }
    group_cols <- intersect(c("household_id", "hbs_year", "weight", "year"), names(dt))
    out <- dt[
      ,
      .(mean_inflation = mean(inflation, na.rm = TRUE)),
      by = group_cols
    ]
    return(out)
  }

  dt <- data.table::as.data.table(x)
  if (!all(c("household_id", "year", "mean_inflation") %in% names(dt))) {
    stop(
      "'x' must be a household_inflation object or contain household_id, year, ",
      "and mean_inflation columns.",
      call. = FALSE
    )
  }
  data.table::copy(dt)
}

household_inflation_histogram_dt <- function(annual_mean, bin_width = 0.1) {
  dt <- data.table::copy(annual_mean)
  dt[, inflation_bin := floor(mean_inflation / bin_width) * bin_width]
  hist_dt <- dt[
    is.finite(mean_inflation) & is.finite(weight) & weight > 0,
    .(weighted_households = sum(weight), n_sample = .N),
    by = .(year, inflation_bin)
  ]
  hist_dt[, share_households := 100 * weighted_households / sum(weighted_households), by = year]
  hist_dt[, bin_mid := inflation_bin + bin_width / 2]
  data.table::setorder(hist_dt, year, inflation_bin)
  hist_dt
}
