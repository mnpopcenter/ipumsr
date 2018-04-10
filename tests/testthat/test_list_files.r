context("Listing files")

# This function helps find the data from the ipumsexamples package for us
ex_file <- function(x) {
  system.file("extdata", x, package = "ipumsexamples")
}

nhgis_csv <- ipums_example("nhgis0008_csv.zip")
nhgis_shp <- ipums_example("nhgis0008_shape_small.zip")
terra_raster <- ex_file("2552_bundle.zip")
terra_micro <- ex_file("3484_bundle.zip")
terra_area <- ex_file("3485_bundle.zip")

test_that("Listing files from nhgis csv zip works", {
  if (!file.exists(nhgis_csv)) {
    skip("Couldn't find nhgis csv.")
  }

  all_files <- ipums_list_files(nhgis_csv)
  expect_equal(all_files$type, "data")
  expect_equal(all_files$file, "nhgis0008_csv/nhgis0008_ds135_1990_pmsa.csv")

  data_files <- ipums_list_data(nhgis_csv)
  expect_equal(data_files$file, "nhgis0008_csv/nhgis0008_ds135_1990_pmsa.csv")

  shape_files <- ipums_list_shape(nhgis_csv)
  expect_equal(nrow(shape_files), 0)

  raster_files <- ipums_list_raster(nhgis_csv)
  expect_equal(nrow(raster_files), 0)
})

test_that("Listing files from nhgis shape zip works", {
  if (!file.exists(nhgis_shp)) {
    skip("Couldn't find nhgis shape")
  }

  all_files <- ipums_list_files(nhgis_shp)
  expect_equal(all_files$type, "shape")
  expect_equal(all_files$file, "nhgis0008_shape/nhgis0008_shapefile_tl2000_us_pmsa_1990.zip")

  shape_files <- ipums_list_shape(nhgis_shp)
  expect_equal(shape_files$file, "nhgis0008_shape/nhgis0008_shapefile_tl2000_us_pmsa_1990.zip")

  data_files <- ipums_list_data(nhgis_shp)
  expect_equal(nrow(data_files), 0)

  raster_files <- ipums_list_raster(nhgis_shp)
  expect_equal(nrow(raster_files), 0)
})

test_that("Listing files from terra raster zip works", {
  if (!file.exists(terra_raster)) {
    skip("Couldn't find terra raster. ipumsexamples likely not installed.")
  }

  all_files <- ipums_list_files(terra_raster)
  expect_true(all(all_files$type == "raster"))
  expect_equal(all_files$file[1], "PASTURE2000ZM2013.tiff")

  data_files <- ipums_list_data(terra_raster)
  expect_equal(nrow(data_files), 0)

  shape_files <- ipums_list_shape(terra_raster)
  expect_equal(nrow(shape_files), 0)

  raster_files <- ipums_list_raster(terra_raster)
  expect_equal(raster_files$file[1], "PASTURE2000ZM2013.tiff")
})

test_that("Listing files from terra area zip works", {
  if (!file.exists(terra_raster)) {
    skip("Couldn't find terra area. ipumsexamples likely not installed.")
  }

  all_files <- ipums_list_files(terra_area)
  expect_equal(all_files$type, c("data", "shape"))
  expect_equal(all_files$file, c("data_3485_AGG_NP_FLAD_1981.csv", "boundaries_3485_AGG_NP_FLAD_1981.zip"))

  data_files <- ipums_list_data(terra_area)
  expect_equal(data_files$file, c("data_3485_AGG_NP_FLAD_1981.csv"))

  shape_files <- ipums_list_shape(terra_area)
  expect_equal(shape_files$file, c("boundaries_3485_AGG_NP_FLAD_1981.zip"))

  raster_files <- ipums_list_raster(terra_area)
  expect_equal(nrow(raster_files), 0)
})

test_that("Listing files from terra micro zip works", {
  if (!file.exists(terra_micro)) {
    skip("Couldn't find terra micro. ipumsexamples likely not installed.")
  }

  all_files <- ipums_list_files(terra_micro)
  expect_equal(all_files$type, c("data", "shape"))
  expect_equal(all_files$file, c("terrapop_extract_3484.csv.gz", "boundaries_3484_IPUMS_SL_FLAD_2004.zip"))

  data_files <- ipums_list_data(terra_micro)
  expect_equal(data_files$file, c("terrapop_extract_3484.csv.gz"))

  shape_files <- ipums_list_shape(terra_micro)
  expect_equal(shape_files$file, c("boundaries_3484_IPUMS_SL_FLAD_2004.zip"))

  raster_files <- ipums_list_raster(terra_micro)
  expect_equal(nrow(raster_files), 0)
})

# macOS unzips when you download, so we should support this
test_that("Can list files from unzipped folder works (terra micro)", {
  if (!file.exists(terra_micro)) {
    skip("Couldn't find terra micro. ipumsexamples likely not installed.")
  }

  temp_dir <- tempdir()
  unzip(terra_micro, exdir = temp_dir)

  all_files <- ipums_list_files(temp_dir)
  expect_equal(all_files$type, c("data", "shape"))
  expect_equal(all_files$file, c("terrapop_extract_3484.csv.gz", "boundaries_3484_IPUMS_SL_FLAD_2004.zip"))

  data_files <- ipums_list_data(temp_dir)
  expect_equal(data_files$file, c("terrapop_extract_3484.csv.gz"))

  shape_files <- ipums_list_shape(temp_dir)
  expect_equal(shape_files$file, c("boundaries_3484_IPUMS_SL_FLAD_2004.zip"))

  raster_files <- ipums_list_raster(temp_dir)
  expect_equal(nrow(raster_files), 0)
})
