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

    # Also should work when you set lower_vars when reading the DDI
    ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"), lower_vars = TRUE)
    data <- read_ipums_micro(ddi, verbose = FALSE)

    expect_equal(names(data), tolower(names(data)))
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

    # Also should work when you set lower_vars when reading the DDI
    ddi <- read_ipums_ddi(ipums_example("cps_00010.xml"), lower_vars = TRUE)
    data <- read_ipums_micro_list(ddi, verbose = FALSE)

    expect_equal(names(data$PERSON), tolower(names(data$PERSON)))
  })

test_that("lower_vars = TRUE warning on rectangular .dat.gz", {
  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))

  expect_warning(
    data <- read_ipums_micro(ddi, verbose = FALSE, lower_vars = TRUE),
    regexp = "has been ignored"
  )
  expect_equal(names(data), toupper(names(data)))
})


test_that("lower_vars = TRUE warning on hierarchical .dat.gz", {
  ddi <- read_ipums_ddi(ipums_example("cps_00010.xml"))

  expect_warning(
    data <- read_ipums_micro_list(ddi, verbose = FALSE, lower_vars = TRUE),
    regexp = "has been ignored"
  )
  expect_equal(names(data$PERSON), toupper(names(data$PERSON)))
})


test_that("lower_vars arg works on .csv.gz file", {
  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"), lower_vars = TRUE)
  data <- read_ipums_micro(
    ddi,
    data_file = ipums_example("cps_00006.csv.gz"),
    verbose = FALSE
  )

  expect_equal(names(data), tolower(names(data)))

  data <- read_ipums_micro(
    ddi = ipums_example("cps_00006.xml"),
    data_file = ipums_example("cps_00006.csv.gz"),
    verbose = FALSE,
    lower_vars = TRUE
  )

  expect_equal(names(data), tolower(names(data)))
})

test_that("lower_vars arg works on chunked rectangular .dat.gz", {
  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))

  expect_warning(
    data <- read_ipums_micro_chunked(
      ddi,
      IpumsDataFrameCallback$new(function(x, ...) x[x$STATEFIP == 19, ]),
      verbose = FALSE,
      lower_vars = TRUE
    ),
    regexp = "has been ignored"
  )
  expect_equal(names(data), toupper(names(data)))

  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"), lower_vars = TRUE)
  data <- read_ipums_micro_chunked(
    ddi,
    IpumsDataFrameCallback$new(function(x, ...) x[x$statefip == 19, ]),
    verbose = FALSE
  )

  expect_equal(names(data), tolower(names(data)))

  data <- read_ipums_micro_chunked(
    ipums_example("cps_00006.xml"),
    IpumsDataFrameCallback$new(function(x, ...) x[x$statefip == 19, ]),
    verbose = FALSE,
    lower_vars = TRUE
  )

  expect_equal(names(data), tolower(names(data)))

})


test_that("lower_vars arg works on chunked hierarchical .dat.gz", {
  ddi <- read_ipums_ddi(ipums_example("cps_00010.xml"))

  expect_warning(
    data <- read_ipums_micro_list_chunked(
      ddi,
      IpumsListCallback$new(function(x, ...) x),
      verbose = FALSE,
      lower_vars = TRUE
    ),
    regexp = "has been ignored"
  )
  data_combined <- list(
    HOUSEHOLD = ipums_bind_rows(
      lapply(data, function(x) x$HOUSEHOLD)
    ),
    PERSON = ipums_bind_rows(
      lapply(data, function(x) x$PERSON)
    )
  )
  expect_equal(names(data_combined$PERSON), toupper(names(data_combined$PERSON)))


  ddi <- read_ipums_ddi(ipums_example("cps_00010.xml"), lower_vars = TRUE)
  data <- read_ipums_micro_list_chunked(
    ddi,
    IpumsListCallback$new(function(x, ...) x),
    verbose = FALSE
  )
  data_combined <- list(
    HOUSEHOLD = ipums_bind_rows(
      lapply(data, function(x) x$HOUSEHOLD)
    ),
    PERSON = ipums_bind_rows(
      lapply(data, function(x) x$PERSON)
    )
  )

  expect_equal(names(data_combined$PERSON), tolower(names(data_combined$PERSON)))

  data <- read_ipums_micro_list_chunked(
    ddi = ipums_example("cps_00010.xml"),
    IpumsListCallback$new(function(x, ...) x),
    verbose = FALSE,
    lower_vars = TRUE
  )
  data_combined <- list(
    HOUSEHOLD = ipums_bind_rows(
      lapply(data, function(x) x$HOUSEHOLD)
    ),
    PERSON = ipums_bind_rows(
      lapply(data, function(x) x$PERSON)
    )
  )

  expect_equal(names(data_combined$PERSON), tolower(names(data_combined$PERSON)))
})

test_that("lower_vars arg works on chunked .csv.gz file", {
  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"), lower_vars = TRUE)
  data <- read_ipums_micro_chunked(
    ddi,
    IpumsDataFrameCallback$new(function(x, ...) x[x$STATEFIP == 19, ]),
    data_file = ipums_example("cps_00006.csv.gz"),
    verbose = FALSE
  )

  expect_equal(names(data), tolower(names(data)))

  data <- read_ipums_micro_chunked(
    ddi = ipums_example("cps_00006.xml"),
    IpumsDataFrameCallback$new(function(x, ...) x[x$STATEFIP == 19, ]),
    data_file = ipums_example("cps_00006.csv.gz"),
    verbose = FALSE,
    lower_vars = TRUE
  )

  expect_equal(names(data), tolower(names(data)))
})

test_that("lower_vars arg works with _yield on rectangular .dat.gz", {
  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))

  expect_warning(
    cps_yield_source <- read_ipums_micro_yield(
      ddi,
      verbose = FALSE,
      lower_vars = TRUE
    ),
    regexp = "has been ignored"
  )
  cps_yield <- list()
  while(!cps_yield_source$is_done()) {
    cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
  }
  data <- ipums_bind_rows(cps_yield)

  expect_equal(names(data), toupper(names(data)))


  ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"), lower_vars = TRUE)
  cps_yield_source <- read_ipums_micro_yield(
    ddi,
    verbose = FALSE
  )
  cps_yield <- list()
  while(!cps_yield_source$is_done()) {
    cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
  }
  data <- ipums_bind_rows(cps_yield)

  expect_equal(names(data), tolower(names(data)))


  cps_yield_source <- read_ipums_micro_yield(
    ddi = ipums_example("cps_00006.xml"),
    verbose = FALSE,
    lower_vars = TRUE
  )
  cps_yield <- list()
  while(!cps_yield_source$is_done()) {
    cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
  }
  data <- ipums_bind_rows(cps_yield)

  expect_equal(names(data), tolower(names(data)))

})


test_that("lower_vars arg works with _yield on hierarchical .dat.gz", {
  ddi <- read_ipums_ddi(ipums_example("cps_00010.xml"))

  expect_warning(
    cps_yield_source <- read_ipums_micro_list_yield(
      ddi,
      verbose = FALSE,
      lower_vars = TRUE
    ),
    regexp = "has been ignored"
  )
  cps_yield <- list()
  while(!cps_yield_source$is_done()) {
    cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
  }
  data <- list(
    HOUSEHOLD = ipums_bind_rows(
      lapply(cps_yield, function(x) x$HOUSEHOLD)
    ),
    PERSON = ipums_bind_rows(
      lapply(cps_yield, function(x) x$PERSON)
    )
  )

  expect_equal(names(data$PERSON), toupper(names(data$PERSON)))


  ddi <- read_ipums_ddi(ipums_example("cps_00010.xml"), lower_vars = TRUE)
  cps_yield_source <- read_ipums_micro_list_yield(
    ddi,
    verbose = FALSE
  )
  cps_yield <- list()
  while(!cps_yield_source$is_done()) {
    cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
  }
  data <- list(
    HOUSEHOLD = ipums_bind_rows(
      lapply(cps_yield, function(x) x$HOUSEHOLD)
    ),
    PERSON = ipums_bind_rows(
      lapply(cps_yield, function(x) x$PERSON)
    )
  )

  expect_equal(names(data$PERSON), tolower(names(data$PERSON)))

  cps_yield_source <- read_ipums_micro_list_yield(
    ddi = ipums_example("cps_00010.xml"),
    verbose = FALSE,
    lower_vars = TRUE
  )
  cps_yield <- list()
  while(!cps_yield_source$is_done()) {
    cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
  }
  data <- list(
    HOUSEHOLD = ipums_bind_rows(
      lapply(cps_yield, function(x) x$HOUSEHOLD)
    ),
    PERSON = ipums_bind_rows(
      lapply(cps_yield, function(x) x$PERSON)
    )
  )

  expect_equal(names(data$PERSON), tolower(names(data$PERSON)))
})
