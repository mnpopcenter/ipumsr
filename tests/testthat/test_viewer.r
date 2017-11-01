context("ipums_view is fault tolerant")

ipums_view_error_check <- function(x) {
  out <- purrr::safely(ipums_view)(x, launch = FALSE)
  out$error
}

test_that("normal ddi doesn't error", {
  ddi <- read_ipums_ddi(ripums_example("cps_00006.xml"))
  attempt <- ipums_view_error_check(ddi)
  expect_true(is.null(attempt), info = paste(attempt, collapse = ";"))
})

test_that("empty ddi doesn't error", {
  ddi <- make_ddi_from_scratch()
  attempt <- ipums_view_error_check(ddi)
  expect_true(is.null(attempt), info = paste(attempt, collapse = ";"))
})

test_that("normal microdata doesn't error", {
  data <- read_ipums_micro(ripums_example("cps_00006.xml"), verbose = FALSE)
  attempt <- ipums_view_error_check(data)
  expect_true(is.null(attempt), info = paste(attempt, collapse = ";"))
})

test_that("attribute-less microdata doesn't error", {
  data <- read_ipums_micro(
    ripums_example("cps_00006.xml"), verbose = FALSE, var_attrs = NULL
  )
  attempt <- ipums_view_error_check(data)
  expect_true(is.null(attempt), info = paste(attempt, collapse = ";"))
})

test_that("nhgis codebook doesn't error", {
  cb <- read_ipums_codebook(ripums_example("nhgis0008_csv.zip"))
  attempt <- ipums_view_error_check(cb)
  expect_true(is.null(attempt), info = paste(attempt, collapse = ";"))
})
