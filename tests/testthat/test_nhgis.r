context("NHGIS")

# Manually set these constants...
rows <- 71
vars_data <- 17
vars_data_shape_sf <- 24
vars_data_shape_sp <- 23
d6z001_label <- "1989 to March 1990"
d6z001_var_desc <- "Year Structure Built (D6Z)"
pmsa_first2_sort <- c("Akron, OH PMSA", "Anaheim--Santa Ana, CA PMSA")

test_that(
  "Can read NHGIS extract (data only)", {
    nhgis <- read_nhgis(
      ipums_example("nhgis0008_csv.zip"),
      verbose = FALSE
    )

    expect_equal(nrow(nhgis), rows)
    expect_equal(ncol(nhgis), vars_data)
    expect_equal(attr(nhgis[["D6Z001"]], "label"), d6z001_label)
    expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), d6z001_var_desc)
    expect_equal(sort(nhgis$PMSA)[1:2], pmsa_first2_sort)
    expect_equal(class(nhgis), c("tbl_df", "tbl", "data.frame"))
  })


test_that(
  "Can read NHGIS extract (with shape as sf)", {
    skip_if_not_installed("sf")
    nhgis <- read_nhgis_sf(
      ipums_example("nhgis0008_csv.zip"),
      ipums_example("nhgis0008_shape_small.zip"),
      verbose = FALSE
    )

    expect_equal(nrow(nhgis), rows)
    expect_equal(ncol(nhgis), vars_data_shape_sf)
    expect_equal(attr(nhgis[["D6Z001"]], "label"), d6z001_label)
    expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), d6z001_var_desc)
    expect_equal(sort(nhgis$PMSA)[1:2], pmsa_first2_sort)
    expect_equal(class(nhgis), c("sf", "tbl_df", "tbl", "data.frame"))
  })


test_that(
  "Can read NHGIS extract (with shape as sf - 1 layer unzipped)", {
    skip_if_not_installed("sf")
    temp_dir <- tempfile()
    dir.create(temp_dir)
    utils::unzip(ipums_example("nhgis0008_csv.zip"), exdir = temp_dir)
    utils::unzip(ipums_example("nhgis0008_shape_small.zip"), exdir = temp_dir)

    nhgis <- read_nhgis_sf(
      file.path(temp_dir, "nhgis0008_csv/nhgis0008_ds135_1990_pmsa.csv"),
      file.path(temp_dir, "nhgis0008_shape/nhgis0008_shapefile_tl2000_us_pmsa_1990.zip"),
      verbose = FALSE
    )

    expect_equal(nrow(nhgis), rows)
    expect_equal(ncol(nhgis), vars_data_shape_sf)
    expect_equal(attr(nhgis[["D6Z001"]], "label"), d6z001_label)
    expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), d6z001_var_desc)
    expect_equal(sort(nhgis$PMSA)[1:2], pmsa_first2_sort)
    expect_equal(class(nhgis), c("sf", "tbl_df", "tbl", "data.frame"))

    nhgis2 <- read_nhgis_sf(
      file.path(temp_dir, "nhgis0008_csv"),
      file.path(temp_dir, "nhgis0008_shape"),
      verbose = FALSE
    )
    expect_equal(nrow(nhgis), nrow(nhgis2))
    expect_equal(ncol(nhgis), ncol(nhgis2))
    expect_equal(attr(nhgis[["D6Z001"]], "label"), attr(nhgis2[["D6Z001"]], "label"))
    expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), attr(nhgis2[["D6Z001"]], "var_desc"))
    expect_equal(sort(nhgis$PMSA)[1:2], sort(nhgis2$PMSA)[1:2])
    expect_equal(class(nhgis), class(nhgis2))
  })


test_that(
  "Can read NHGIS extract (with shape as sf - 2 layers unzipped)", {
    skip_if_not_installed("sf")
    temp_dir <- tempfile()
    dir.create(temp_dir)
    utils::unzip(ipums_example("nhgis0008_csv.zip"), exdir = temp_dir)
    utils::unzip(ipums_example("nhgis0008_shape_small.zip"), exdir = temp_dir)
    utils::unzip(
      file.path(temp_dir, "nhgis0008_shape/nhgis0008_shapefile_tl2000_us_pmsa_1990.zip"),
      exdir = temp_dir
    )

    nhgis <- read_nhgis_sf(
      file.path(temp_dir, "nhgis0008_csv/nhgis0008_ds135_1990_pmsa.csv"),
      file.path(temp_dir, "US_pmsa_1990.shp"),
      verbose = FALSE
    )

    expect_equal(nrow(nhgis), rows)
    expect_equal(ncol(nhgis), vars_data_shape_sf)
    expect_equal(attr(nhgis[["D6Z001"]], "label"), d6z001_label)
    expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), d6z001_var_desc)
    expect_equal(sort(nhgis$PMSA)[1:2], pmsa_first2_sort)
    expect_equal(class(nhgis), c("sf", "tbl_df", "tbl", "data.frame"))
  })


test_that(
  "Can read NHGIS extract (with shape as sp)", {
    skip_if_not_installed("rgdal")
    skip_if_not_installed("sp")
    nhgis <- read_nhgis_sp(
      ipums_example("nhgis0008_csv.zip"),
      ipums_example("nhgis0008_shape_small.zip"),
      verbose = FALSE
    )

    expect_equal(nrow(nhgis@data), rows)
    expect_equal(ncol(nhgis@data), vars_data_shape_sp)
    # Attributes get killed by sp::merge... I'm okay with supporting sf better than sp
    #expect_equal(attr(nhgis[["D6Z001"]], "label"), d6z001_label)
    #expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), d6z001_var_desc)
    expect_equal(sort(nhgis$PMSA)[1:2], pmsa_first2_sort)
    expect_equal(class(nhgis), rlang::set_attrs("SpatialPolygonsDataFrame", package = "sp"))
  })

test_that(
  "NHGIS - sf and sp polygon-data relationships didn't get scrambled in import", {
    skip_if_not_installed("sf")
    skip_if_not_installed("rgdal")
    skip_if_not_installed("sp")

    nhgis_sf <- read_nhgis_sf(
      ipums_example("nhgis0008_csv.zip"),
      ipums_example("nhgis0008_shape_small.zip"),
      verbose = FALSE
    )

    nhgis_sp <- read_nhgis_sp(
      ipums_example("nhgis0008_csv.zip"),
      ipums_example("nhgis0008_shape_small.zip"),
      verbose = FALSE
    )

    check_geo <- nhgis_sf$GISJOIN[1]
    expect_equal(
      dplyr::filter(nhgis_sf, GISJOIN == check_geo)$geometry[[1]][[1]][[1]],
      subset(nhgis_sp, GISJOIN == check_geo)@polygons[[1]]@Polygons[[1]]@coords
    )

  })


test_that(
  "Informative error when no data file", {
    expect_error(read_nhgis("FAKE_FILE.zip"), "working directory")
    expect_error(read_nhgis("C:/FAKE_FOLDER/FAKE_FILE.zip"), "check the path")
  })
