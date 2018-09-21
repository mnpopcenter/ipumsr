context("Lower variable names")
test_that(
  "Reading variables in lowercase is okay - DDI", {
    ddi_regular <- read_ipums_ddi(ipums_example("cps_00006.xml"))
    ddi_lower <- read_ipums_ddi(ipums_example("cps_00006.xml"), lower_vars = TRUE)

    expect_equal(
      tolower(ddi_regular$var_info$var_name),
      ddi_lower$var_info$var_name
    )

    var_info_regular <- ipums_var_info(ddi_regular, "INCTOT")
    var_info_lower <- ipums_var_info(ddi_lower, "inctot")

    var_info_regular$var_name <- tolower(var_info_regular$var_name)
    expect_equal(
      var_info_regular$val_labels,
      var_info_lower$val_labels
    )

    var_info_regular$val_labels <- NULL
    var_info_lower$val_labels <- NULL

    expect_equal(var_info_regular, var_info_lower)

  })

test_that(
  "Reading variables in lowercase is okay - rect micro data", {
    rect_regular <- read_ipums_micro(
      ipums_example("cps_00006.xml"),
      verbose = FALSE
    )
    rect_lower <- read_ipums_micro(
      ipums_example("cps_00006.xml"),
      lower_vars = TRUE,
      verbose = FALSE
    )

    # Data is the same except for names
    names(rect_regular) <- tolower(names(rect_regular))
    expect_equal(rect_regular, rect_lower)
  })

test_that(
  "Reading variables in lowercase is okay - hier micro data", {
    hier_regular <- read_ipums_micro_list(
      ipums_example("cps_00010.xml"),
      verbose = FALSE
    )
    hier_lower <- read_ipums_micro_list(
      ipums_example("cps_00010.xml"),
      lower_vars = TRUE,
      verbose = FALSE
    )

    # Data is the same except for names
    names(hier_regular[[1]]) <- tolower(names(hier_regular[[1]]))
    names(hier_regular[[2]]) <- tolower(names(hier_regular[[2]]))
    expect_equal(hier_regular, hier_lower)
  })
