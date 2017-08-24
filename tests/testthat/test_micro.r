context("Microdata")

# Manually set these constants...
rows_h <- 3385
rows_p <- 7668

vars_h <- 5
vars_p <- 5
vars_all <- 9
vars_rect <- 8

YEAR_label <- "Survey year"
YEAR_var_desc <- "YEAR reports the year in which the survey was conducted.  YEARP is repeated on person records."
STATEFIP_val_labels <- c(Alabama = 1, Alaska = 2)

test_that(
  "Can read Rectangular .dat.gz", {
    skip_if_not_installed("ripumstest")
    cps <- read_ipums_micro(
      ripums_example("cps_00006.xml"),
      data_file = ripums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )

    expect_equal(nrow(cps), rows_p)
    expect_equal(ncol(cps), vars_rect)
    expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  })

test_that(
  "Can read Rectangular .csv.gz", {
    skip_if_not_installed("ripumstest")
    cps <- read_ipums_micro(
      ripums_example("cps_00006.xml"),
      data_file = ripums_example("cps_00006.csv.gz"),
      verbose = FALSE
    )

    expect_equal(nrow(cps), rows_p)
    expect_equal(ncol(cps), vars_rect)
    expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  })

test_that(
  "Can read Hierarchical into long format", {
    skip_if_not_installed("ripumstest")
    cps <- read_ipums_micro(
      ripums_example("cps_00010.xml"),
      data_structure = "long",
      verbose = FALSE
    )

    expect_equal(nrow(cps), rows_h + rows_p)
    expect_equal(ncol(cps), vars_all)
    expect_equal(attr(cps[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  })

test_that(
  "Can read Hierarchical into list format", {
    skip_if_not_installed("ripumstest")
    cps <- read_ipums_micro(
      ripums_example("cps_00010.xml"),
      data_structure = "list",
      verbose = FALSE
    )

    expect_equal(nrow(cps$H), rows_h)
    expect_equal(nrow(cps$P), rows_p)
    expect_equal(ncol(cps$H), vars_h)
    expect_equal(ncol(cps$P), vars_p)
    expect_equal(attr(cps$H[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps$P[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps$H[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps$P[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps$H[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  })


test_that(
  "Arguments n_max and vars work", {
    skip_if_not_installed("ripumstest")
    cps <- read_ipums_micro(
      ripums_example("cps_00010.xml"),
      n_max = 100,
      vars = c(RECTYPE, STATEFIP),
      data_structure = "long",
      verbose = FALSE
    )
    expect_equal(nrow(cps), 100)
    expect_equal(ncol(cps), 2)
  })

