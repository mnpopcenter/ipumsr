context("ipums_view is fault tolerant")

ipums_view_error_check <- function(x) {
  out <- purrr::safely(ipums_view)(x, launch = FALSE)
  is.null(out$error)
}

test_that("normal ddi doesn't error", {
  ddi <- read_ipums_ddi(ripums_example("cps_00006.xml"))
  expect_true(ipums_view_error_check(ddi))
})

test_that("empty ddi doesn't error", {
  ddi <- make_ddi_from_scratch()
  expect_true(ipums_view_error_check(ddi))
})

test_that("normal microdata doesn't error", {
  data <- read_ipums_micro(ripums_example("cps_00006.xml"), verbose = FALSE)
  expect_true(ipums_view_error_check(data))
})

test_that("attribute-less microdata doesn't error", {
  data <- read_ipums_micro(
    ripums_example("cps_00006.xml"), verbose = FALSE, var_attrs = NULL
  )
  expect_true(ipums_view_error_check(data))
})

test_that("nhgis codebook doesn't error", {
  cb <- read_ipums_codebook(ripums_example("nhgis0008_csv.zip"))
  expect_true(ipums_view_error_check(cb))
})
