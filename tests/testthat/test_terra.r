# Very basic tests for now
context("IPUMS Terra")

# This function helps find the data from the ipumsexamples package for us
ex_file <- function(x) {
  system.file("extdata", x, package = "ipumsexamples")
}

test_that("Terra raster works", {
  raster_file <- ex_file("2552_bundle.zip")
  if (!file.exists(raster_file)) {
    skip("Couldn't find raster example file. ipumsexamples likely not installed.")
  }
  skip_if_not_installed("raster")
  raster <- read_terra_raster_list(raster_file, verbose = FALSE)

  expect_equal(length(raster), 22)
  expect_s4_class(raster[[1]], "RasterLayer")
})

test_that("Terra micro works", {
  micro_file <- ex_file("3484_bundle.zip")
  if (!file.exists(micro_file)) {
    skip("Couldn't find micro example file. ipumsexamples likely not installed.")
  }
  skip_if_not_installed("sf")

  micro <- read_terra_micro(micro_file, verbose = FALSE)

  expect_s3_class(micro$COUNTRY, "labelled")
})

test_that("Terra area works (sf)", {
  area_file <- ex_file("3485_bundle.zip")
  if (!file.exists(area_file)) {
    skip("Couldn't find area example file. ipumsexamples likely not installed.")
  }
  skip_if_not_installed("sf")

  area <- read_terra_area_sf(area_file, verbose = FALSE)
  expect_equal(attr(area$GEOG_CODE_LABEL, "label"), "Name of geographic instances")
  expect_s3_class(area, "sf")
  expect_equal(attr(area, "sf_column"), "geometry")
})

test_that("Terra area works (sp)", {
  area_file <- ex_file("3485_bundle.zip")
  if (!file.exists(area_file)) {
    skip("Couldn't find area example file. ipumsexamples likely not installed.")
  }
  skip_if_not_installed("rgdal")
  skip_if_not_installed("sp")

  area <- read_terra_area_sp(area_file, verbose = FALSE)
  expect_equal(class(area), rlang::set_attrs("SpatialPolygonsDataFrame", package = "sp"))
})
