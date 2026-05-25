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

inflation_obj <- function(country, category = "income", level = 2,
                          start_year = 2019, end_year = NULL,
                          end_month = NULL, ...) {
  key <- paste(
    "inflation", country, category, level, start_year,
    end_year %||% "NULL", end_month %||% "NULL",
    paste(names(list(...)), list(...), collapse = "_"),
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
    paste(names(list(...)), list(...), collapse = "_"),
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
    base_year %||% "NULL", paste(names(list(...)), list(...), collapse = "_"),
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

euro_area_income_hbs <- function(level = 2, start_year = 2020, end_year = 2026,
                                 aggregate_geo = "EA20") {
  key <- paste("ea_hbs", level, start_year, end_year, aggregate_geo, sep = "_")
  cached(key, function() {
    country_weights <- load_country_weights(
      aggregate_geo = aggregate_geo,
      start_year = start_year,
      end_year = end_year
    )
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

figure_registry <- list(
  list(
    order = 1,
    file = "figure1.png",
    caption = "Inflation developments, euro area",
    status = "todo",
    builder = todo("No package-native euro-area headline figure builder yet.")
  ),
  list(
    order = 2,
    file = "figure2.png",
    caption = "Consumption baskets by income quintiles, euro area",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area basket aggregation.")
  ),
  list(
    order = 3,
    file = "fig_inflation_ea_income.png",
    caption = "Short-run inflation inequality in the euro area by income quintile",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area income aggregation.")
  ),
  list(
    order = 4,
    file = "fig_inflation_ea_age.png",
    caption = "Inflation by age in the euro area",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area age aggregation.")
  ),
  list(
    order = 5,
    file = "fig_inflation_ea_urban.png",
    caption = "Inflation by residence area in the euro area",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area residence aggregation.")
  ),
  list(
    order = 6,
    file = "fig_index_ea_income.png",
    caption = "Long-run inflation inequality in the euro area by income quintile",
    status = "todo",
    builder = todo("Requires a paper-specific euro-area long-run aggregation.")
  ),
  list(
    order = 7,
    file = "figure4.png",
    caption = "Inflation by income quintiles, Baltic countries, 2022",
    status = "implemented",
    builder = function() {
      p <- plot_grouped_bar(
        2022,
        inflation_obj("EE", start_year = 2019, end_year = 2022),
        inflation_obj("LV", start_year = 2019, end_year = 2022),
        inflation_obj("LT", start_year = 2019, end_year = 2022)
      )
      save_paper_plot(p, "figure4.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 8,
    file = "figure5.png",
    caption = "Euro-area HICP price index for first and fifth income quintiles",
    status = "implemented",
    builder = function() {
      indices <- price_indices_obj(
        "EA",
        "income",
        level = 2,
        start_year = 2020,
        end_year = 2026,
        end_month = 4,
        custom_hbs = euro_area_income_hbs(level = 2, start_year = 2020, end_year = 2026)
      )
      p <- plot_group_price_indices(
        indices,
        categories = c("First quintile", "Fifth quintile"),
        include_total = FALSE
      ) +
        labs(
          title = "Euro-area HICP price index by income quintile",
          subtitle = "Q1 and Q5, using euro-area HICP item prices and weights",
          y = "Index level"
        )
      save_paper_plot(p, "figure5.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 9,
    file = "figure6.png",
    caption = "Inflation by income quintiles, Germany, Spain, France, 2022",
    status = "implemented",
    builder = function() {
      p <- plot_grouped_bar(
        2022,
        inflation_obj("DE", start_year = 2019, end_year = 2022),
        inflation_obj("ES", start_year = 2019, end_year = 2022),
        inflation_obj("FR", start_year = 2019, end_year = 2022)
      )
      save_paper_plot(p, "figure6.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 10,
    file = "figure7.png",
    caption = "Inflation by income quintiles, Austria, Finland, Luxembourg, 2022",
    status = "implemented",
    builder = function() {
      p <- plot_grouped_bar(
        2022,
        inflation_obj("AT", start_year = 2019, end_year = 2022),
        inflation_obj("FI", start_year = 2019, end_year = 2022),
        inflation_obj("LU", start_year = 2019, end_year = 2022)
      )
      save_paper_plot(p, "figure7.png", width = 8, height = 5.2)
    }
  ),
  list(
    order = 11,
    file = "figure9.png",
    caption = "Inflation inequality developments in Spain: inflation by income quintile",
    status = "implemented",
    builder = function() {
      p <- plot_time_series(inflation_obj("ES", start_year = 2019))
      save_paper_plot(p, "figure9.png", width = 8, height = 5)
    }
  ),
  list(
    order = 12,
    file = "figure10.png",
    caption = "Inflation inequality developments in Spain: contribution to inflation inequality",
    status = "implemented",
    builder = function() {
      p <- plot_contribution_gap(contributions_obj("ES", start_year = 2019))
      save_paper_plot(p, "figure10.png", width = 8, height = 5)
    }
  ),
  list(
    order = 13,
    file = "figure13.png",
    caption = "Inflation in France: actual and counterfactual",
    status = "todo",
    builder = todo("Requires the paper-specific counterfactual scenario definition.")
  ),
  list(
    order = 14,
    file = "figA_DE_2000.png",
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
      save_paper_plot(p, "figA_DE_2000.png", width = 8, height = 5)
    }
  ),
  list(
    order = 15,
    file = "figA_DE_2020.png",
    caption = "Analysis with fixed expenditure shares: rolling HBS waves",
    status = "implemented",
    builder = function() {
      p <- plot_time_series(inflation_obj("DE", start_year = 2000, end_year = 2024))
      save_paper_plot(p, "figA_DE_2020.png", width = 8, height = 5)
    }
  ),
  list(
    order = 16,
    file = "figureB.png",
    caption = "Comparison with national CPI: France",
    status = "implemented",
    builder = function() {
      fr <- inflation_obj("FR", start_year = 2019, end_year = 2026, end_month = 3)
      p <- compare_to_official_hicp(fr)$plot
      save_paper_plot(p, "figureB.png", width = 8, height = 5)
    }
  ),
  list(
    order = 17,
    file = "figureC.png",
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
      save_paper_plot(p, "figureC.png", width = 8, height = 5)
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

run_exports()
