context("Microdata")

# Manually set these constants...
rows_h <- 3385
rows_p <- 7668

vars_h <- 6
vars_p <- 6
vars_all <- 9
vars_rect <- 8

YEAR_label <- "Survey year"
YEAR_var_desc <- "YEAR reports the year in which the survey was conducted.  YEARP is repeated on person records."
STATEFIP_val_labels <- c(Alabama = 1, Alaska = 2)

test_that(
  "Can read Rectangular .dat.gz", {
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
    cps <- read_ipums_micro(
      ripums_example("cps_00010.xml"),
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
    cps <- read_ipums_micro_list(
      ripums_example("cps_00010.xml"),
      verbose = FALSE
    )

    expect_equal(nrow(cps$HOUSEHOLD), rows_h)
    expect_equal(nrow(cps$PERSON), rows_p)
    expect_equal(ncol(cps$HOUSEHOLD), vars_h)
    expect_equal(ncol(cps$PERSON), vars_p)
    expect_equal(attr(cps$HOUSEHOLD[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps$PERSON[["YEAR"]], "label"), YEAR_label)
    expect_equal(attr(cps$HOUSEHOLD[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps$PERSON[["YEAR"]], "var_desc"), YEAR_var_desc)
    expect_equal(attr(cps$HOUSEHOLD[["STATEFIP"]], "labels")[1:2], STATEFIP_val_labels)
  })


test_that(
  "Arguments n_max and vars work", {
    cps <- read_ipums_micro(
      ripums_example("cps_00010.xml"),
      n_max = 100,
      vars = c(RECTYPE, STATEFIP),
      verbose = FALSE
    )
    expect_equal(nrow(cps), 100)
    expect_equal(ncol(cps), 2)
  })



test_that(
  "Setting argument var_attrs to NULL works", {
    cps <- read_ipums_micro(
      ripums_example("cps_00006.xml"),
      data_file = ripums_example("cps_00006.dat.gz"),
      verbose = FALSE,
      var_attrs = NULL
    )

    no_var_attrs <- purrr::map_lgl(cps, ~is.null(attributes(.)))
    expect_true(all(no_var_attrs))
  })


test_that(
  "Informative error when no ddi file", {
    expect_error(read_ipums_micro("FAKE_FILE.xml"), "working directory")
    expect_error(read_ipums_micro("C:/FAKE_FOLDER/FAKE_FILE.xml"), "check the path")
  })
