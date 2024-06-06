test_that("CPI data formatted as data.table", {
    f <- load_cpi("FR", 3)
    expect(data.table::is.data.table(dt))
})

test_that("loading French CPI data", {
    dt <- load_cpi("FR", 3)
    expect_equal(colnames(dt), c("series_name", "coicop", "value",
                                 "year", "yearmonth"))
})

test_that("loading German CPI data", {
    dt <- load_cpi("DE", 3)
    expect_equal(colnames(dt), c("series_name", "coicop", "value",
                                 "year", "yearmonth"))
})

test_that("HBS data formatted as data.table", {
    dt <- load_hbs("FR", 3)
    expect(data.table::is.data.table(dt))
})

test_that("loading French HBS data", {
    dt <- load_hbs("FR", 3)
    expect_equal(colnames(dt),
                 c("coicop", "quantile", "consumption"))
    expect_in("TOT", unique(dt$quantile))
})
