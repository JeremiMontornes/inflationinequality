test_that("plot_weight_shares returns a ggplot object", {
  weights <- structure(
    list(
      dt = data.table::data.table(
        coicop = rep(c("011", "012", "021"), each = 2),
        year = 2020,
        category = rep(c("Q1", "Q5"), times = 3),
        weighted_consumption = c(45, 25, 35, 50, 20, 25),
        weight_year = 2024
      ),
      categories = c("Q1", "Q5"),
      country = "FR",
      category = "income",
      level = 2
    ),
    class = "weights"
  )

  plot <- plot_weight_shares(weights, weight_year = 2024, top_n = 2)

  expect_s3_class(plot, "ggplot")
  expect_true("product_label" %in% names(plot$data))
  expect_true(all(c("Food", "Non-alcoholic beverages", "Other") %in% levels(plot$data$product_label)))
  expect_equal(anyDuplicated(levels(plot$data$product_label)), 0)
  expect_false(any(levels(plot$data$product_label) %in% c("011", "012", "021")))
})

test_that("plot_weight_shares validates inputs", {
  expect_error(plot_weight_shares(data.frame()), "calculate_weights")
})
