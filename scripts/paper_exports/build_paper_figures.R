#!/usr/bin/env Rscript

# Export paper figures from package calculations, following their order in main.tex.

script_file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_file)) sub("^--file=", "", script_file[[1]]) else
  "scripts/paper_exports/build_paper_figures.R"
package_root <- normalizePath(file.path(dirname(script_file), "..", ".."), mustWork = FALSE)
paper_repo <- Sys.getenv(
  "INFLATIONINEQUALITY_PAPER_REPO",
  file.path(dirname(package_root), "inflation-inequality-paper")
)
fig_dir <- file.path(paper_repo, "fig")
manifest_path <- file.path(fig_dir, "_paper_figure_exports_manifest.csv")

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

if (requireNamespace("pkgload", quietly = TRUE) &&
    file.exists(file.path(package_root, "DESCRIPTION"))) {
  pkgload::load_all(package_root, quiet = TRUE)
} else {
  library(inflationinequality)
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

cache <- new.env(parent = emptyenv())

cached <- function(key, value) {
  if (!exists(key, envir = cache, inherits = FALSE)) {
    assign(key, value(), envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)
}

cache_args_key <- function(...) {
  args <- list(...)
  if (!length(args)) {
    return("")
  }
  paste(
    names(args),
    vapply(args, function(x) {
      if (inherits(x, "hbs")) {
        return(paste(
          "hbs", x$country, x$category, x$level,
          paste(x$categories, collapse = "+"),
          sep = ":"
        ))
      }
      if (length(x) == 1 && is.atomic(x)) {
        return(as.character(x))
      }
      paste(class(x)[1], length(x), sep = ":")
    }, character(1)),
    collapse = "_"
  )
}

inflation_obj <- function(country, category = "income", level = 2,
                          start_year = 2019, end_year = NULL,
                          end_month = NULL, ...) {
  key <- paste(
    "inflation", country, category, level, start_year,
    end_year %||% "NULL", end_month %||% "NULL",
    cache_args_key(...),
    sep = "_"
  )
  cached(key, function() {
    calculate_inflation(
      country,
      category,
      level = level,
      start_year = start_year,
      end_year = end_year,
      end_month = end_month,
      ...
    )
  })
}

contributions_obj <- function(country, category = "income", level = 2,
                              start_year = 2019, end_year = NULL,
                              end_month = NULL, ...) {
  key <- paste(
    "contributions", country, category, level, start_year,
    end_year %||% "NULL", end_month %||% "NULL",
    cache_args_key(...),
    sep = "_"
  )
  cached(key, function() {
    calculate_contributions(
      country,
      category,
      level = level,
      start_year = start_year,
      end_year = end_year,
      end_month = end_month,
      ...
    )
  })
}

price_indices_obj <- function(country, category = "income", level = 2,
                              start_year = 2019, end_year = NULL,
                              end_month = NULL, base_year = NULL, ...) {
  key <- paste(
    "indices", country, category, level, start_year,
    end_year %||% "NULL", end_month %||% "NULL",
    base_year %||% "NULL", cache_args_key(...),
    sep = "_"
  )
  cached(key, function() {
    calculate_price_indices(
      country,
      category,
      level = level,
      start_year = start_year,
      end_year = end_year,
      end_month = end_month,
      base_year = base_year,
      ...
    )
  })
}

euro_area_income_hbs <- function(level = 2, start_year = 2000, end_year = 2026,
                                 aggregate_geo = "EA20") {
  key <- paste("ea_hbs", level, start_year, end_year, aggregate_geo, sep = "_")
  cached(key, function() {
    country_weights <- load_country_weights(
      aggregate_geo = aggregate_geo,
      start_year = start_year,
      end_year = end_year
    )
    country_weights[, country_weight := country_weight / sum(country_weight), by = year]
    countries <- sort(unique(country_weights$country))

    hbs_list <- lapply(countries, function(country_i) {
      hbs_i <- load_hbs(country_i, "income", level = level)
      list(country = country_i, hbs = hbs_i)
    })

    dt <- rbindlist(lapply(hbs_list, function(x) {
      out <- data.table::copy(x$hbs$dt)
      out[, country := x$country]
      out
    }), use.names = TRUE, fill = TRUE)
    dt_total <- rbindlist(lapply(hbs_list, function(x) {
      out <- data.table::copy(x$hbs$dt_total)
      out[, country := x$country]
      out
    }), use.names = TRUE, fill = TRUE)

    target_years <- sort(unique(country_weights$year))
    dt_expanded <- rbindlist(lapply(target_years, function(weight_year) {
      dt[
        ,
        {
          candidates <- .SD[year <= weight_year]
          if (nrow(candidates) > 0) candidates[which.max(year)] else .SD[which.min(year)]
        },
        by = .(country, coicop, category)
      ][, weight_year := weight_year]
    }), use.names = TRUE, fill = TRUE)
    dt_total_expanded <- rbindlist(lapply(target_years, function(weight_year) {
      dt_total[
        ,
        {
          candidates <- .SD[year <= weight_year]
          if (nrow(candidates) > 0) candidates[which.max(year)] else .SD[which.min(year)]
        },
        by = .(country, coicop)
      ][, weight_year := weight_year]
    }), use.names = TRUE, fill = TRUE)

    dt_expanded <- merge(
      dt_expanded,
      country_weights[, .(country, weight_year = year, country_weight)],
      by = c("country", "weight_year")
    )
    dt_total_expanded <- merge(
      dt_total_expanded,
      country_weights[, .(country, weight_year = year, country_weight)],
      by = c("country", "weight_year")
    )

    ea_dt <- dt_expanded[
      ,
      .(
        series_name = paste(unique(stats::na.omit(series_name)), collapse = "; "),
        consumption = sum(consumption * country_weight, na.rm = TRUE)
      ),
      by = .(coicop, year = weight_year, category)
    ]
    ea_dt[series_name == "", series_name := NA_character_]
    setcolorder(ea_dt, c("series_name", "coicop", "year", "category", "consumption"))

    ea_dt_total <- dt_total_expanded[
      ,
      .(total_consumption = sum(total_consumption * country_weight, na.rm = TRUE)),
      by = .(coicop, year = weight_year)
    ]

    hbs(
      dt = ea_dt,
      dt_total = ea_dt_total,
      country = "EA",
      category = "income",
      categories = category_data$income$categories,
      level = level
    )
  })
}

save_paper_plot <- function(plot, file, width = 8, height = 5, dpi = 300) {
  out <- file.path(fig_dir, file)
  ggplot2::ggsave(out, plot, width = width, height = height, dpi = dpi, bg = "white")
  out
}

todo <- function(reason) {
  force(reason)
  function() {
    stop(reason, call. = FALSE)
  }
}

ea_hicp_component_rates <- function(start = "2014-01", end = NULL) {
  components <- c(
    ELC_GAS = "Electricity, gas",
    FOOD = "Food",
    FUEL = "Fuel",
    IGD_NNRG = "Non-energy industrial goods",
    SERV = "Services"
  )
  latest_complete_month <- format(as.Date(format(Sys.Date(), "%Y-%m-01")) - 1, "%Y-%m")
  date_range <- c(start, end %||% latest_complete_month)
  filters <- list(
    freq = "M",
    unit = "RCH_A",
    geo = "EA",
    coicop18 = names(components)
  )

  dt_raw <- download_hicp_dataset(
    id = "prc_hicp_minr",
    filters = filters,
    date.range = date_range
  )

  dt <- data.table::as.data.table(dt_raw)
  if (nrow(dt) == 0 || !all(c("time", "values", "coicop18") %in% names(dt))) {
    stop("Eurostat HICP component data could not be downloaded.", call. = FALSE)
  }

  dt <- dt[
    coicop18 %chin% names(components),
    .(
      date = as.Date(paste0(time, "-01")),
      component = components[coicop18],
      value = as.numeric(values)
    )
  ]
  dt <- dt[date >= as.Date(paste0(start, "-01")) & !is.na(value)]
  dt[, component := factor(component, levels = unname(components))]
  dt[order(component, date)]
}

plot_ea_hicp_components <- function() {
  dt <- ea_hicp_component_rates()
  value_range <- range(dt$value, na.rm = TRUE)
  label_y <- value_range[2] + diff(value_range) * 0.08

  ggplot(dt, aes(x = date, y = value, color = component)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_vline(
      xintercept = as.Date(c("2020-03-01", "2022-02-01")),
      color = "grey20",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    annotate(
      "text",
      x = as.Date("2020-03-15"),
      y = label_y,
      label = "Covid-19",
      hjust = 0,
      size = 3.5
    ) +
    annotate(
      "text",
      x = as.Date("2022-02-15"),
      y = label_y,
      label = "Ukraine war",
      hjust = 0,
      size = 3.5
    ) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", expand = expansion(mult = c(0.01, 0.01))) +
    scale_color_manual(
      values = c(
        "Electricity, gas" = "#F8766D",
        "Food" = "#6B8E23",
        "Fuel" = "#00A878",
        "Non-energy industrial goods" = "#7B5EA7",
        "Services" = "#C77CFF"
      )
    ) +
    coord_cartesian(ylim = c(min(0, value_range[1]), label_y + diff(value_range) * 0.05)) +
    labs(
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank()
    )
}

fr_level3_end_year <- 2026
fr_level3_end_month <- 3

fr_level3_inflation <- function(category, start_year = 2019) {
  args <- list(
    country = "FR",
    category = category,
    level = 3,
    start_year = start_year,
    end_year = fr_level3_end_year,
    end_month = fr_level3_end_month
  )
  if (identical(category, "income")) {
    args$france_insee_income_groups <- "quintile"
  }
  do.call(inflation_obj, args)
}

fr_level3_contributions <- function(category, start_year = 2019) {
  args <- list(
    country = "FR",
    category = category,
    level = 3,
    start_year = start_year,
    end_year = fr_level3_end_year,
    end_month = fr_level3_end_month
  )
  if (identical(category, "income")) {
    args$france_insee_income_groups <- "quintile"
  }
  do.call(contributions_obj, args)
}

fr_level3_category_mapping <- list(
  "Alcoholic beverage, tobacco and narcotics" = c("02"),
  "Clothing and footwear" = c("03"),
  "Food and non-alcoholic beverages" = c("01"),
  "Housing (rentals and repairs)" = c("041", "042", "043", "05"),
  "Transport" = c("07"),
  "Water, electricity, gas and other fuels" = c("044", "045")
)

paper_naked_plot <- function(plot) {
  plot +
    labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, caption = NULL) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.caption = element_blank()
    )
}

plot_fr_level3_contribution_gap <- function(contributions,
                                            low = contributions$categories[1],
                                            high = contributions$categories[length(contributions$categories)],
                                            line_label = NULL,
                                            reverse = FALSE) {
  if (!all(c(low, high) %in% contributions$categories)) {
    stop(
      "Requested categories not found in contributions: ",
      paste(setdiff(c(low, high), contributions$categories), collapse = ", "),
      call. = FALSE
    )
  }

  map_coicop_to_category <- generate_coicop_mapping(fr_level3_category_mapping)
  dt_reorganized <- data.table::copy(
    contributions$dt[
      category %in% c(low, high),
      .(year, month, category, coicop, contribution)
    ]
  )
  dt_reorganized[, COICOP_category := map_coicop_to_category(coicop)]
  dt_aggregated <- dt_reorganized[
    ,
    .(contribution = sum(contribution)),
    by = .(year, month, category, COICOP_category)
  ]
  dt_wide <- data.table::dcast(
    dt_aggregated,
    year + month + COICOP_category ~ category,
    value.var = "contribution"
  )
  if (isTRUE(reverse)) {
    dt_gap <- dt_wide[
      ,
      .(
        year = year,
        month = month,
        COICOP_category = COICOP_category,
        contribution_gap = get(high) - get(low)
      )
    ]
    line_label <- line_label %||% sprintf("Inflation gap: %s minus %s", high, low)
  } else {
    dt_gap <- dt_wide[
      ,
      .(
        year = year,
        month = month,
        COICOP_category = COICOP_category,
        contribution_gap = get(low) - get(high)
      )
    ]
    line_label <- line_label %||% sprintf("Inflation gap: %s minus %s", low, high)
  }
  dt_gap[, date := as.Date(sprintf("%04d-%02d-01", year, month))]
  dt_total <- dt_gap[, .(total_gap = sum(contribution_gap)), by = .(year, month, date)]

  ggplot() +
    geom_col(aes(date, contribution_gap, fill = COICOP_category), data = dt_gap) +
    geom_line(
      aes(date, total_gap, color = line_label),
      data = dt_total,
      linewidth = 1.1
    ) +
    scale_color_manual(values = stats::setNames("black", line_label)) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    labs(
      title = "",
      x = NULL,
      y = NULL,
      fill = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    guides(
      fill = guide_legend(ncol = 3),
      color = guide_legend(order = 1)
    ) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank()
    )
}

plot_fr_level3_q1_q5_price_indices <- function() {
  probe <- calculate_price_indices(
    "FR",
    "income",
    level = 3,
    start_year = NULL,
    end_year = fr_level3_end_year,
    end_month = fr_level3_end_month,
    france_insee_income_groups = "quintile",
    include_total = FALSE
  )
  first_year <- probe$start_year
  indices <- price_indices_obj(
    "FR",
    "income",
    level = 3,
    start_year = first_year,
    end_year = fr_level3_end_year,
    end_month = fr_level3_end_month,
    base_year = first_year,
    france_insee_income_groups = "quintile",
    include_total = FALSE
  )
  dt <- data.table::copy(indices$dt[category %in% c("First quintile", "Fifth quintile")])
  dt[, category := factor(
    category,
    levels = c("First quintile", "Fifth quintile"),
    labels = c("Q1", "Q5")
  )]

  ggplot(dt, aes(date, price_index, color = category, group = category)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = c("Q1" = "#D73027", "Q5" = "#4575B4")) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    labs(
      title = NULL,
      x = NULL,
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

figure_registry <- list(
  list(
    order = 1,
    file = "fig_EA_headline_inflation.png",
    caption = "Inflation developments, euro area",
    status = "todo",
    builder = todo("No package-native euro-area headline figure builder yet.")
  ),
  list(
    order = 2,
    file = "fig_EA_hicp_component.png",
    caption = "Euro-area HICP annual inflation by component",
    status = "implemented",
    builder = function() {
      p <- plot_ea_hicp_components()
      save_paper_plot(p, "fig_EA_hicp_component.png", width = 8, height = 6)
    }
  ),
  list(
    order = 3,
    file = "fig_EA_income_baskets.png",
    caption = "Consumption baskets by income quintiles, euro area",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area basket aggregation.")
  ),
  list(
    order = 4,
    file = "fig_EA_income_inflation_short_run.png",
    caption = "Short-run inflation inequality in the euro area by income quintile",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area income aggregation.")
  ),
  list(
    order = 5,
    file = "fig_EA_age_inflation.png",
    caption = "Inflation by age in the euro area",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area age aggregation.")
  ),
  list(
    order = 6,
    file = "fig_EA_urban_inflation.png",
    caption = "Inflation by residence area in the euro area",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area residence aggregation.")
  ),
  list(
    order = 7,
    file = "fig_EA_income_price_index_long_run.png",
    caption = "Long-run inflation inequality in the euro area by income quintile",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area long-run aggregation.")
  ),
  list(
    order = 8,
    file = "fig_Baltics_income_inflation_2022.png",
    caption = "Inflation by income quintiles, Baltic countries, 2022",
    status = "implemented",
    builder = function() {
      p <- plot_grouped_bar(
        2022,
        inflation_obj("EE", start_year = 2019, end_year = 2022),
        inflation_obj("LV", start_year = 2019, end_year = 2022),
        inflation_obj("LT", start_year = 2019, end_year = 2022)
      )
      save_paper_plot(p, "fig_Baltics_income_inflation_2022.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 9,
    file = "fig_EA_hicp_price_index.png",
    caption = "Euro-area HICP price index",
    status = "implemented",
    builder = function() {
      indices <- price_indices_obj(
        "EA",
        "income",
        level = 2,
        start_year = 2000,
        end_year = 2026,
        end_month = 4
      )
      p <- plot_group_price_indices(
        indices,
        categories = "Total",
        include_total = TRUE
      ) +
        labs(
          title = "Euro-area HICP price index",
          subtitle = "calculate_price_indices(\"EA\", \"income\", level = 2, start_year = 2000, end_year = 2026, end_month = 4)",
          y = "Index level"
        ) +
        scale_x_date(date_labels = "%Y", date_breaks = "5 years")
      save_paper_plot(p, "fig_EA_hicp_price_index.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 10,
    file = "fig_DE_ES_FR_income_inflation_2022.png",
    caption = "Inflation by income quintiles, Germany, Spain, France, 2022",
    status = "implemented",
    builder = function() {
      p <- plot_grouped_bar(
        2022,
        inflation_obj("DE", start_year = 2019, end_year = 2022),
        inflation_obj("ES", start_year = 2019, end_year = 2022),
        inflation_obj("FR", start_year = 2019, end_year = 2022)
      )
      save_paper_plot(p, "fig_DE_ES_FR_income_inflation_2022.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 11,
    file = "fig_AT_FI_LU_income_inflation_2022.png",
    caption = "Inflation by income quintiles, Austria, Finland, Luxembourg, 2022",
    status = "implemented",
    builder = function() {
      p <- plot_grouped_bar(
        2022,
        inflation_obj("AT", start_year = 2019, end_year = 2022),
        inflation_obj("FI", start_year = 2019, end_year = 2022),
        inflation_obj("LU", start_year = 2019, end_year = 2022)
      )
      save_paper_plot(p, "fig_AT_FI_LU_income_inflation_2022.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 12,
    file = "fig_ES_income_inflation_timeseries.png",
    caption = "Inflation inequality developments in Spain: inflation by income quintile",
    status = "implemented",
    builder = function() {
      p <- plot_time_series(inflation_obj("ES", start_year = 2019))
      save_paper_plot(p, "fig_ES_income_inflation_timeseries.png", width = 8, height = 5)
    }
  ),
  list(
    order = 13,
    file = "fig_ES_income_contribution_gap.png",
    caption = "Inflation inequality developments in Spain: contribution to inflation inequality",
    status = "implemented",
    builder = function() {
      p <- plot_contribution_gap(contributions_obj("ES", start_year = 2019))
      save_paper_plot(p, "fig_ES_income_contribution_gap.png", width = 8, height = 5)
    }
  ),
  list(
    order = 14,
    file = "fig_FR_counterfactual_inflation.png",
    caption = "Inflation in France: actual and counterfactual",
    status = "todo",
    builder = todo("Requires the paper-specific counterfactual scenario definition.")
  ),
  list(
    order = 15,
    file = "fig_DE_income_inflation_fixed_hbs_2000.png",
    caption = "Analysis with fixed expenditure shares: fixed weights",
    status = "implemented",
    builder = function() {
      p <- plot_time_series(
        inflation_obj(
          "DE",
          start_year = 2000,
          end_year = 2024,
          specific_hbs_year = 2000
        )
      )
      save_paper_plot(p, "fig_DE_income_inflation_fixed_hbs_2000.png", width = 8, height = 5)
    }
  ),
  list(
    order = 16,
    file = "fig_DE_income_inflation_rolling_hbs.png",
    caption = "Analysis with fixed expenditure shares: rolling HBS waves",
    status = "implemented",
    builder = function() {
      p <- plot_time_series(inflation_obj("DE", start_year = 2000, end_year = 2024))
      save_paper_plot(p, "fig_DE_income_inflation_rolling_hbs.png", width = 8, height = 5)
    }
  ),
  list(
    order = 17,
    file = "fig_FR_hicp_validation.png",
    caption = "Comparison with national CPI: France",
    status = "implemented",
    builder = function() {
      fr <- inflation_obj("FR", start_year = 2019, end_year = 2026, end_month = 3)
      p <- compare_to_official_hicp(fr)$plot
      save_paper_plot(p, "fig_FR_hicp_validation.png", width = 8, height = 5)
    }
  ),
  list(
    order = 18,
    file = "fig_FR_coicop_level_validation.png",
    caption = "Effect of COICOP level: France",
    status = "implemented",
    builder = function() {
      level2 <- price_indices_obj(
        "FR", level = 2, start_year = 2010, end_year = 2026,
        end_month = 3, base_year = 2010
      )
      level3 <- price_indices_obj(
        "FR", level = 3, start_year = 2010, end_year = 2026,
        end_month = 3, base_year = 2010,
        france_insee_income_groups = "quintile"
      )
      c2 <- compare_to_official_hicp(level2)$dt
      c3 <- compare_to_official_hicp(level3)$dt
      c2[, level := "COICOP level 2"]
      c3[, level := "COICOP level 3"]
      dt <- rbindlist(list(c2, c3), use.names = TRUE, fill = TRUE)
      p <- ggplot(dt, aes(date, difference, color = level)) +
        geom_hline(yintercept = 0, color = "grey40", linewidth = 0.3) +
        geom_line(linewidth = 0.8, na.rm = TRUE) +
        scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
        labs(
          x = NULL,
          y = "Calculated minus published HICP (index points)",
          color = NULL,
          title = "Effect of COICOP level on French HICP validation"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")
      save_paper_plot(p, "fig_FR_coicop_level_validation.png", width = 8, height = 5)
    }
  ),
  list(
    order = 19,
    file = "fig_FR_income_inflation_level3_Q1_Q5.png",
    caption = "Additional figure A: France level-3 inflation by income quintile",
    status = "implemented",
    builder = function() {
      p <- paper_naked_plot(plot_time_series(fr_level3_inflation("income")))
      save_paper_plot(p, "fig_FR_income_inflation_level3_Q1_Q5.png", width = 8, height = 5)
    }
  ),
  list(
    order = 20,
    file = "fig_FR_age_inflation_level3_under25_65plus.png",
    caption = "Additional figure A: France level-3 inflation by age group",
    status = "implemented",
    builder = function() {
      p <- paper_naked_plot(plot_time_series(fr_level3_inflation("age")))
      save_paper_plot(p, "fig_FR_age_inflation_level3_under25_65plus.png", width = 8, height = 5)
    }
  ),
  list(
    order = 21,
    file = "fig_FR_urban_inflation_level3_rural_paris.png",
    caption = "Additional figure A: France level-3 inflation by residence area",
    status = "implemented",
    builder = function() {
      p <- paper_naked_plot(plot_time_series(fr_level3_inflation("urban")))
      save_paper_plot(p, "fig_FR_urban_inflation_level3_rural_paris.png", width = 8, height = 5)
    }
  ),
  list(
    order = 22,
    file = "fig_FR_income_contribution_gap_level3_Q1_Q5.png",
    caption = "Additional figure A: France level-3 contributions to the Q1-Q5 inflation gap",
    status = "implemented",
    builder = function() {
      p <- plot_fr_level3_contribution_gap(
        fr_level3_contributions("income"),
        low = "First quintile",
        high = "Fifth quintile",
        line_label = "Inflation gap: Q1 minus Q5"
      )
      save_paper_plot(p, "fig_FR_income_contribution_gap_level3_Q1_Q5.png", width = 8, height = 5)
    }
  ),
  list(
    order = 23,
    file = "fig_FR_age_contribution_gap_level3_65plus_under25.png",
    caption = "Additional figure A: France level-3 contributions to the 65+ minus under-25 inflation gap",
    status = "implemented",
    builder = function() {
      p <- plot_fr_level3_contribution_gap(
        fr_level3_contributions("age"),
        low = "Under 25 years",
        high = "65 years or over",
        line_label = "Inflation gap: 65 years or over minus under 25",
        reverse = TRUE
      )
      save_paper_plot(p, "fig_FR_age_contribution_gap_level3_65plus_under25.png", width = 8, height = 5)
    }
  ),
  list(
    order = 24,
    file = "fig_FR_urban_contribution_gap_level3_rural_paris.png",
    caption = "Additional figure A: France level-3 contributions to the rural-Paris inflation gap",
    status = "implemented",
    builder = function() {
      p <- plot_fr_level3_contribution_gap(
        fr_level3_contributions("urban"),
        low = "Rural areas",
        high = "Paris",
        line_label = "Inflation gap: rural areas minus Paris"
      )
      save_paper_plot(p, "fig_FR_urban_contribution_gap_level3_rural_paris.png", width = 8, height = 5)
    }
  ),
  list(
    order = 25,
    file = "fig_FR_income_price_index_level3_Q1_Q5_first_available.png",
    caption = "Additional figure A: France level-3 Q1-Q5 price indices from first available year",
    status = "implemented",
    builder = function() {
      p <- plot_fr_level3_q1_q5_price_indices()
      save_paper_plot(p, "fig_FR_income_price_index_level3_Q1_Q5_first_available.png", width = 8, height = 5)
    }
  )
)

run_exports <- function(registry = figure_registry, overwrite = TRUE) {
  manifest <- rbindlist(lapply(registry, function(item) {
    out_file <- file.path(fig_dir, item$file)
    if (!isTRUE(overwrite) && file.exists(out_file)) {
      return(data.table(
        order = item$order, file = item$file, caption = item$caption,
        status = "kept_existing", message = "File exists and overwrite is FALSE."
      ))
    }
    if (!identical(item$status, "implemented")) {
      return(data.table(
        order = item$order, file = item$file, caption = item$caption,
        status = "skipped", message = "No package-native builder yet."
      ))
    }
    result <- tryCatch(
      {
        item$builder()
        data.table(
          order = item$order, file = item$file, caption = item$caption,
          status = "exported", message = normalizePath(out_file, mustWork = FALSE)
        )
      },
      error = function(e) {
        data.table(
          order = item$order, file = item$file, caption = item$caption,
          status = "failed", message = conditionMessage(e)
        )
      }
    )
    result
  }), use.names = TRUE, fill = TRUE)

  fwrite(manifest, manifest_path)
  print(manifest)
  invisible(manifest)
}

if (!identical(Sys.getenv("INFLATIONINEQUALITY_SKIP_RUN_EXPORTS"), "true")) {
  run_exports()
}
