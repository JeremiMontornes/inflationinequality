test_that("Spain EPF household parser builds a household HBS object", {
  skip_if_not_installed("zip")

  tmp <- tempdir()
  root <- file.path(tmp, "epf_fixture")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "hogar", "CSV"), recursive = TRUE)
  dir.create(file.path(root, "gastos", "CSV"), recursive = TRUE)

  data.table::fwrite(
    data.table::data.table(
      ANOENC = 2020L,
      NUMERO = 1:2,
      FACTOR = c(1, 2),
      UC1 = c(1, 2),
      IMPEXAC = c(10000, 30000),
      EDADSP = c(35, 70),
      DENSIDAD = c(1L, 3L)
    ),
    file.path(root, "hogar", "CSV", "EPFhogar_2020.csv"),
    sep = "\t"
  )
  data.table::fwrite(
    data.table::data.table(
      ANOENC = 2020L,
      NUMERO = c(1L, 1L, 2L),
      CODIGO = c("011100", "045100", "011100"),
      GASTO = c(100000, 50000, 200000)
    ),
    file.path(root, "gastos", "CSV", "EPFgastos_2020.csv"),
    sep = "\t"
  )

  hogar_zip <- file.path(root, "EPFhogar_2020.zip")
  gastos_zip <- file.path(root, "EPFgastos_2020.zip")
  old_wd <- setwd(file.path(root, "hogar"))
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(hogar_zip, "CSV/EPFhogar_2020.csv", mode = "cherry-pick")
  setwd(file.path(root, "gastos"))
  zip::zip(gastos_zip, "CSV/EPFgastos_2020.csv", mode = "cherry-pick")
  setwd(root)
  outer_zip <- file.path(tmp, "datos_2020.zip")
  zip::zip(outer_zip, c("EPFhogar_2020.zip", "EPFgastos_2020.zip"), mode = "cherry-pick")

  epf <- inflationinequality:::load_spain_epf_household_microdata(outer_zip)

  expect_s3_class(epf$hbs, "hbs")
  expect_equal(epf$hbs$country, "ES")
  expect_equal(epf$hbs$category, "household")
  expect_equal(epf$hbs$level, 3)
  expect_setequal(epf$hbs$categories, c("ES_2020_1", "ES_2020_2"))
  expect_true(all(c("0111", "0451") %in% epf$hbs$dt$coicop))
  expect_true("01" %in% epf$hbs$dt$coicop)
  expect_equal(epf$households[household_id == "ES_2020_1", age_ref], "From 30 to 44 years")
})

test_that("calculate_household_inflation validates missing EPF zips", {
  zip_dir <- file.path(tempdir(), "missing_epf_zips")
  unlink(zip_dir, recursive = TRUE, force = TRUE)

  expect_error(
    calculate_household_inflation(years = 2020, zip_dir = zip_dir, download = FALSE),
    "Missing INE EPF ZIP"
  )
})

test_that("plot_household_inflation_distribution returns a ggplot", {
  annual_mean <- data.table::data.table(
    household_id = rep(c("h1", "h2", "h3"), times = 2),
    year = rep(c(2021, 2022), each = 3),
    weight = c(1, 2, 1, 1, 2, 1),
    mean_inflation = c(1.1, 1.2, 1.8, 4.8, 5.0, 5.4)
  )

  overlay <- plot_household_inflation_distribution(annual_mean, bin_width = 0.1)
  facets <- plot_household_inflation_distribution(annual_mean, bin_width = 0.1, overlay = FALSE)

  expect_s3_class(overlay, "ggplot")
  expect_s3_class(facets, "ggplot")
})
