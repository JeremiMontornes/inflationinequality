calculate_inflation <- function(country, category, level = 2,
                                start_year = NULL, start_month = NULL,
                                end_year = NULL, end_month = NULL) {
  dt_contributions <- calculate_contributions(country, category,
    level = level,
    start_year = start_year, start_month = start_month,
    end_year = end_year, end_month = end_month
  )
  dt_inflation <- dt_contributions[, .(inflation = sum(contribution)), by = .(year, month, category)]
}

calculate_inflation_gap <- function(dt_inflation) {
  # Ensure dt_inflation is a data.table
  data.table::setDT(dt_inflation)

  # Filter for QUINTILE1 and QUINTILE5, then dcast to wide format
  dt_wide <- data.table::dcast(dt_inflation[category %in% c("QUINTILE1", "QUINTILE5")],
    year + month ~ category,
    value.var = "inflation"
  )

  # Calculate inflation gap
  dt_gap <- dt_wide[, .(
    year = year,
    month = month,
    inflation_gap = QUINTILE1 - QUINTILE5
  )]

  return(dt_gap)
}
