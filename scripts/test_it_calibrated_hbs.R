suppressPackageStartupMessages({
  library(data.table)
  devtools::load_all("C:/Users/jerem/Documents/GitHub/inflationinequality", quiet = TRUE)
})

repo <- "C:/Users/jerem/Documents/GitHub/inflationinequality"
consumption_hbs <- readRDS(file.path(
  repo, "inst", "extdata", "IT_consumption_quintile_hbs_2015_2020_level2.rds"
))

run_case <- function(label, category, custom_hbs = NULL) {
  message("Testing ", label)
  x <- calculate_inflation(
    country = "IT", category = category, level = 2,
    start_year = 2020, start_month = 1,
    end_year = 2020, end_month = 12,
    custom_hbs = custom_hbs,
    weighting_method = "ras"
  )
  stopifnot(nrow(x$dt) > 0L, all(is.finite(x$dt$inflation)))
  data.table(
    structure = label,
    rows = nrow(x$dt),
    categories = uniqueN(x$dt$category),
    first_period = min(sprintf("%04d-%02d", x$dt$year, x$dt$month)),
    last_period = max(sprintf("%04d-%02d", x$dt$year, x$dt$month)),
    min_inflation = min(x$dt$inflation),
    max_inflation = max(x$dt$inflation)
  )
}

results <- rbindlist(list(
  run_case("income_calibrated", "income"),
  run_case("age_clean", "age"),
  run_case("consumption_quintile", "income", consumption_hbs)
))
print(results)
fwrite(results, file.path(
  repo, "data-raw", "italy_calibrated_hbs", "IT_inflation_smoke_test.csv"
))
