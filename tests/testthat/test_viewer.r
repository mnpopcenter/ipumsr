context("ipums_view is fault tolerant")

test_that("normal ddi doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))
  ipums_view(ddi, launch = FALSE)
  expect_true(TRUE)
})

test_that("empty ddi doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  ddi <- make_ddi_from_scratch()
  ipums_view(ddi, launch = FALSE)
  expect_true(TRUE)
})

test_that("normal microdata doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  data <- read_ipums_micro(ipums_example("cps_00006.xml"), verbose = FALSE)
  ipums_view(data, launch = FALSE)
  expect_true(TRUE)
})

test_that("attribute-less microdata doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  data <- read_ipums_micro(
    ipums_example("cps_00006.xml"), verbose = FALSE, var_attrs = NULL
  )
  ipums_view(data, launch = FALSE)
  expect_true(TRUE)
})

test_that("nhgis codebook doesn't error", {
  skip_if_not_installed("htmltools")
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  cb <- read_ipums_codebook(ipums_example("nhgis0008_csv.zip"))
  ipums_view(cb, launch = FALSE)
  expect_true(TRUE)
})
