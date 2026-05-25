test_that("plot_group_price_indices returns a ggplot object", {
  indices <- structure(
    list(
      dt = data.table::data.table(
        category = rep(c("Q1", "Q5", "Total"), each = 3),
        year = 2020,
        month = rep(1:3, times = 3),
        date = rep(as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")), times = 3),
        laspeyres = c(100, 101, 102, 100, 102, 103, 100, 101.5, 102.5),
        chain_laspeyres = c(100, 101, 102, 100, 102, 103, 100, 101.5, 102.5),
        price_index = c(100, 101, 102, 100, 102, 103, 100, 101.5, 102.5),
        annual_rate = NA_real_
      ),
      categories = c("Q1", "Q5"),
      country = "FR",
      category = "income",
      level = 2
    ),
    class = "price_indices"
  )

  plot <- plot_group_price_indices(indices)

  expect_s3_class(plot, "ggplot")
  expect_equal(levels(plot$data$category), c("Q1", "Q5", "Total"))
  expect_equal(plot$labels$y, "Price index (base 100)")
})

test_that("plot_group_price_indices filters categories and validates inputs", {
  indices <- structure(
    list(
      dt = data.table::data.table(
        category = rep(c("Q1", "Q5", "Total"), each = 2),
        year = 2020,
        month = rep(1:2, times = 3),
        price_index = c(100, 101, 100, 102, 100, 101.5)
      ),
      categories = c("Q1", "Q5")
    ),
    class = "price_indices"
  )

  plot <- plot_group_price_indices(indices, categories = "Q1", include_total = FALSE)

  expect_equal(unique(as.character(plot$data$category)), "Q1")
  expect_false("Total" %in% plot$data$category)
  expect_error(plot_group_price_indices(data.frame()), "calculate_price_indices")
  expect_error(plot_group_price_indices(indices, include_total = NA), "TRUE or FALSE")
})
