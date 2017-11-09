context("ipums_view is fault tolerant")

test_that("normal ddi doesn't error", {
  ddi <- read_ipums_ddi(ripums_example("cps_00006.xml"))
  ipums_view(ddi, launch = FALSE)
  expect_true(TRUE)
})

test_that("empty ddi doesn't error", {
  ddi <- make_ddi_from_scratch()
  ipums_view(ddi, launch = FALSE)
  expect_true(TRUE)
})

test_that("normal microdata doesn't error", {
  data <- read_ipums_micro(ripums_example("cps_00006.xml"), verbose = FALSE)
  ipums_view(data, launch = FALSE)
  expect_true(TRUE)
})

test_that("attribute-less microdata doesn't error", {
  data <- read_ipums_micro(
    ripums_example("cps_00006.xml"), verbose = FALSE, var_attrs = NULL
  )
  ipums_view(data, launch = FALSE)
  expect_true(TRUE)
})

test_that("nhgis codebook doesn't error", {
  cb <- read_ipums_codebook(ripums_example("nhgis0008_csv.zip"))
  ipums_view(cb, launch = FALSE)
  expect_true(TRUE)
})
