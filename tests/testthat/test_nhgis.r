context("NHGIS")

# Manually set these constants...
rows <- 71
vars_data <- 17
vars_data_shape <- 24
d6z001_label <- "1989 to March 1990"
d6z001_var_desc <- "Year Structure Built (D6Z)"
pmsa_first2_sort <- c("Akron, OH PMSA", "Anaheim--Santa Ana, CA PMSA")

test_that(
  "Can read NHGIS extract (data only)", {
    nhgis <- read_nhgis(
      ripums_example("nhgis0008_csv.zip"),
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
    nhgis <- read_nhgis(
      ripums_example("nhgis0008_csv.zip"),
      ripums_example("nhgis0008_shape.zip"),
      verbose = FALSE
    )

    expect_equal(nrow(nhgis), rows)
    expect_equal(ncol(nhgis), vars_data_shape)
    expect_equal(attr(nhgis[["D6Z001"]], "label"), d6z001_label)
    expect_equal(attr(nhgis[["D6Z001"]], "var_desc"), d6z001_var_desc)
    expect_equal(sort(nhgis$PMSA)[1:2], pmsa_first2_sort)
    expect_equal(class(nhgis), c("sf", "tbl_df", "tbl", "data.frame"))
  })
