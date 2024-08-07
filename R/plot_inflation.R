#' Plot time series of inflation data
#'
#' @description
#' `plot_time_series()` creates a line plot of inflation rates over time for
#' selected categories.
#'
#' @details
#' The function performs the following operations:
#' 1. Selects the lowest and highest categories from the inflation data.
#' 2. Calculates the average inflation for each (year, month) pair to create a
#' "Total" category.
#' 3. Combines the original data with the new "Total" category.
#' 4. Creates a line plot using ggplot2 with date on the x-axis and inflation
#' rate on the y-axis.
#'
#' @param inflation An object of class `"inflation"` containing inflation data.
#'
#' @returns A ggplot object representing the time series of inflation rates.
#'
#' @examples
#' # Calculate inflation in France from 2020
#' inflation <- calculate_inflation("FR", "income", start_year = 2020)
#'
#' # Plot the time series
#' plot_time_series(inflation)
#'
#' @export
plot_time_series <- function(inflation) {

  lowest_category <- inflation$categories[1]
  highest_category <- inflation$categories[length(inflation$categories)]

  pruned_categories <- c(lowest_category, highest_category, "Total")

  dt <- inflation$dt

  # Calculate average inflation for each (year, month) pair
  total_rows <- dt[, .(inflation = mean(inflation),
                       category = "Total"),
                   by = .(year, month)]

  # Combine original data with new TOTAL rows
  dt <- data.table::rbindlist(list(dt, total_rows), use.names=TRUE)

  dt <- dt[category %in% pruned_categories, ]

  ggplot2::ggplot(dt, ggplot2::aes(x = as.Date(ISOdate(year, month, 1)), y = inflation)) +
    ggplot2::geom_line(ggplot2::aes(color = lowest_category), data = dt[category == lowest_category]) +
    ggplot2::geom_line(ggplot2::aes(color = highest_category), data = dt[category == highest_category]) +
    ggplot2::geom_line(ggplot2::aes(color = "Total"), data = dt[category == "Total"]) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    ggplot2::labs(
      color = "",
      title = "",
      x = "",
      y = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_manual(
      values = c("red", "darkgreen", "black"),
      breaks = pruned_categories
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12))
}

#' Create a grouped bar chart of inflation data
#'
#' @description
#' `plot_grouped_bar()` creates a grouped bar chart comparing inflation rates
#' across different categories and countries for a specific year.
#'
#' @details
#' The function performs the following operations:
#' 1. Combines inflation data from multiple countries into a single data table.
#' 2. Calculates the average inflation for each country and category.
#' 3. Creates a grouped bar chart using ggplot2 with categories on the x-axis
#' and average inflation on the y-axis, grouped by country.
#'
#' @param year The specific year for which to plot the data.
#' @param ... One or more objects of class `"inflation"` containing inflation
#'   data for different countries.
#'
#' @return A ggplot object representing the grouped bar chart of inflation rates.
#'
#' @examples
#' # Calculate inflation in France from 2020
#' inflation_fr <- calculate_inflation("FR", "income", start_year = 2020)
#'
#' # Calculate inflation in Germany from 2020
#' inflation_de <- calculate_inflation("DE", "income", start_year = 2020)
#'
#' # Plot a grouped bar chart comparing 2020 inflation between France and Germany by income
#' plot_grouped_bar(2020, inflation_fr, inflation_de)
#'
#' @export
plot_grouped_bar <- function(year, ...) {
  inflation_list <- list(...)
  categories_order <- inflation_list[[1]]$categories
  # Assuming your list of inflation objects is called 'inflation_list'
  dt <- data.table::rbindlist(
    lapply(inflation_list, function(x) {
      # Create a copy of the data.table and add the country column
      dt_copy <- data.table::copy(x$dt[year == year])
      dt_copy[, country := x$country]
      return(dt_copy)
    }),
    use.names = TRUE
  )
  # Input validation
  dt <- dt[, .(avg_inflation = mean(inflation)), by = .(country, category)]
  dt[, category := factor(category, levels = categories_order)]
  ggplot2::ggplot(dt, mapping = ggplot2::aes(fill = country, y = avg_inflation, x = category)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(color = "",  # Removes legend label (assuming fill is used for color)
                  title = "",  # Removes plot title (optional)
                  x = "",      # Removes x-axis label
                  y = "") +  # Removes y-axis label
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

# contribution to inflation gap
# Alcoholic beverage, tobacco and narcotics (CP02)
# Clothing and footwear (CP03)
# Food and non-alcoholic beverages (CP01)
# Housing (rentals and repairs) (CP041+CP042+CP043+CP05)
# Transport (CP07)
# Water, electricity, gas and other fuels (CP044+CP045)
# Other
# Function to map COICOP codes to new categories
map_coicop_to_category <- function(coicop) {
  categories <- rep("Other", length(coicop))

  categories[substr(coicop, 1, 2) == "02"] <- "Alcoholic beverage, tobacco and narcotics"
  categories[substr(coicop, 1, 2) == "03"] <- "Clothing and footwear"
  categories[substr(coicop, 1, 2) == "01"] <- "Food and non-alcoholic beverages"
  categories[substr(coicop, 1, 2) %in% c("04", "05") &
               substr(coicop, 1, 3) %in% c("041", "042", "043", "05")] <-
    "Housing (rentals and repairs)"
  categories[substr(coicop, 1, 2) == "07"] <- "Transport"
  categories[substr(coicop, 1, 3) %in% c("044", "045")] <-
    "Water, electricity, gas and other fuels"

  return(categories)
}

#' Generate COICOP category mapping
#'
#' @param category_mapping A list where keys are new category names and values are vectors of COICOP codes
#' @return A function that maps COICOP codes to new categories
generate_coicop_mapping <- function(category_mapping) {
  function(coicop) {
    categories <- rep("Other", length(coicop))

    for (new_category in names(category_mapping)) {
      coicop_codes <- category_mapping[[new_category]]
      for (code in coicop_codes) {
        categories[startsWith(coicop, code)] <- new_category
      }
    }

    return(categories)
  }
}

#' Plot contribution to inflation gap
#'
#' @description
#' `plot_contribution_gap()` creates a stacked bar chart showing the
#' contribution of different COICOP categories to the inflation gap between the
#' lowest and highest income categories over time.
#'
#' @details
#' The function performs the following operations:
#' 1. Filters data for the lowest and highest categories.
#' 2. Maps COICOP codes to broader categories.
#' 3. Aggregates contributions by the new categories.
#' 4. Calculates the gap between the lowest and highest categories.
#' 5. Creates a stacked bar chart with date on the x-axis and contribution gap
#' on the y-axis, colored by COICOP category.
#' 6. Overlays a line representing the total gap between inflation rates of
#' bottom and top quintiles.
#'
#' @param contributions an object of class "contributions" containing
#'   contribution data.
#' @param category_mapping a list where keys are new category names and values
#'   are vectors of COICOP codes.
#'
#' @return A ggplot object representing the stacked bar chart of contribution
#'   gaps.
#'
#' @examples
#' # Calculate contribution to inflation in Spain at COICOP level 2 from 2019
#' contributions <- calculate_contributions("ES", "income", level = 2,
#' start_year = 2019)
#'
#' # Plot the contribution to inflation gap
#' plot_contribution_gap(contributions)
#'
#' @export
plot_contribution_gap <- function(contributions, category_mapping = NULL) {
  if (is.null(category_mapping)) {
    # Default mapping if none provided
    category_mapping <- list(
      "Alcoholic beverage, tobacco and narcotics" = c("02"),
      "Clothing and footwear" = c("03"),
      "Food and non-alcoholic beverages" = c("01"),
      "Housing (rentals and repairs)" = c("041", "042", "043", "05"),
      "Transport" = c("07"),
      "Water, electricity, gas and other fuels" = c("044", "045")
    )
  }

  map_coicop_to_category <- generate_coicop_mapping(category_mapping)

  lowest_category <- contributions$categories[1]
  highest_category <- contributions$categories[length(contributions$categories)]

  # Filter for QUINTILE1 and QUINTILE5, then reorganize categories
  dt_reorganized <- contributions$dt[category %in% c(lowest_category, highest_category),
                                     .(year, month, category, coicop, contribution)]

  # Apply the new category mapping
  dt_reorganized[, COICOP_category := map_coicop_to_category(coicop)]

  # Aggregate contributions by new categories
  dt_aggregated <- dt_reorganized[, .(contribution = sum(contribution)),
                                  by = .(year, month, category, COICOP_category)]

  # Calculate the gap
  dt_wide <- data.table::dcast(dt_aggregated, year + month + COICOP_category ~ category, value.var = "contribution")
  dt_gap <- dt_wide[, .(
    year = year,
    month = month,
    COICOP_category = COICOP_category,
    contribution_gap = get(lowest_category) - get(highest_category)
  )]

  # Add date column
  dt_gap[, date := lubridate::ymd(paste(year, month, "01", sep = "-"))]

  # Calculate total gap
  dt_total <- dt_gap[, .(total_gap = sum(contribution_gap)), by = .(year, month, date)]

  # Plot
  ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = date, y = contribution_gap, fill = COICOP_category),
                      data = dt_gap, stat = "identity") +
    ggplot2::geom_line(ggplot2::aes(x = date, y = total_gap,
                                    color = "Difference between inflation rates of bottom and top quintiles"),
                       data = dt_total, size = 2, stat = "identity") +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    ggplot2::scale_color_manual(values = c("Difference between inflation rates of bottom and top quintiles" = "black")) +
    ggplot2::labs(color = "", title = "", x = "", y = "") +
    ggplot2::theme_minimal() +
    ggplot2::guides(
      fill = ggplot2::guide_legend(ncol = 3),  # Change this line
      color = ggplot2::guide_legend(order = 1)  # Ensure color legend appears first
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.box = "vertical"  # Stack color and fill legends vertically
    )
}

#' Plot Inflation Gap for Multiple Countries
#'
#' @description
#' `plot_inflation_gap()` creates a line plot showing the inflation gap over
#' time for multiple countries. The inflation gap is typically the difference
#' between the inflation rates of the lowest and highest income categories.
#'
#' @details
#' The function performs the following operations:
#' 1. Calculates the inflation gap for each provided country using the
#' `calculate_inflation_gap` function.
#' 2. Combines the data from all countries into a single data table.
#' 3. Converts year and month information into a date format.
#' 4. Creates a line plot with date on the x-axis and inflation gap on the
#' y-axis, with different colors for each country.
#'
#' @param ... One or more objects containing inflation data for different
#'   countries. Each object should be compatible with the
#'   `calculate_inflation_gap` function.
#'
#' @return A ggplot object representing the line plot of inflation gaps for
#'   multiple countries.
#'
#' @examples
#' # Assuming you have inflation data for Spain (ES) and France (FR)
#' es_inflation <- calculate_inflation("ES", "income", level = 2, start_year = 2019)
#' fr_inflation <- calculate_inflation("FR", "income", level = 2, start_year = 2019)
#'
#' # Plot the inflation gap for both countries
#' plot_inflation_gap(es_inflation, fr_inflation)
#'
#' @export
plot_inflation_gap <- function(...) {
  inflation_list <- list(...)

  # Calculate inflation gaps for each country
  inflation_gaps <- lapply(inflation_list, calculate_inflation_gap)

  # Combine all data.tables into one
  dt <- data.table::rbindlist(inflation_gaps, idcol = "country")

  # Convert year and month to date
  dt[, date := lubridate::ymd(paste(year, month, "01", sep = "-"))]

  # Create the plot
  ggplot2::ggplot(dt, ggplot2::aes(x = date, y = inflation_gap, color = country, group = country)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    ggplot2::labs(color = "",
                  title = "n",
                  x = "",
                  y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12))
}
