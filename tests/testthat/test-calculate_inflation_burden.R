test_that("calculate_inflation_burden computes income burden and cost", {
  inflation <- structure(
    list(
      dt = data.table::data.table(
        year = c(2019, 2020, 2019, 2020),
        month = c(12, 1, 12, 1),
        category = c("Q1", "Q1", "Q5", "Q5"),
        inflation = c(4, 5, 2, 3)
      ),
      country = "FR",
      category = "income",
      categories = c("Q1", "Q5"),
      level = 2
    ),
    class = "inflation"
  )

  consumption_to_income <- data.table::data.table(
    category = c("Q1", "Q1", "Q5", "Q5"),
    year = c(2015, 2020, 2015, 2020),
    consumption_to_income = c(100, 110, 50, 55)
  )
  expenditure <- data.table::data.table(
    category = c("Q1", "Q1", "Q5", "Q5"),
    year = c(2015, 2020, 2015, 2020),
    expenditure = c(10000, 12000, 30000, 33000)
  )

  burden <- calculate_inflation_burden(
    inflation,
    consumption_to_income = consumption_to_income,
    expenditure = expenditure
  )

  expect_s3_class(burden, "inflation_burden")
  expect_equal(burden$dt[category == "Q1" & year == 2019, inflation_burden], 4)
  expect_equal(burden$dt[category == "Q1" & year == 2020, inflation_burden], 5.5)
  expect_equal(burden$dt[category == "Q5" & year == 2019, inflation_cost], 600)
  expect_equal(burden$dt[category == "Q5" & year == 2020, inflation_cost], 990)
})

test_that("plot_inflation_burden returns a ggplot object", {
  burden <- structure(
    list(
      dt = data.table::data.table(
        date = as.Date(c("2020-01-01", "2020-02-01")),
        year = 2020,
        month = 1:2,
        category = c("Q1", "Q1"),
        inflation = c(4, 5),
        consumption_to_income = 100,
        inflation_burden = c(4, 5)
      ),
      categories = "Q1",
      expenditure_unit = NULL
    ),
    class = "inflation_burden"
  )

  plot <- plot_inflation_burden(burden)

  expect_s3_class(plot, "ggplot")
  expect_equal(plot$labels$y, "Inflation burden (% of disposable income)")
})

test_that("calculate_inflation_burden validates inputs", {
  expect_error(calculate_inflation_burden(data.frame()), "calculate_inflation")
})
