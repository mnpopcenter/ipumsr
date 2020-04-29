context("Microdata - yield")

test_that(
  "basic yield reading - long .dat.gz rect matches chunked", {
    cps_yield_source <- read_ipums_micro_yield(
      ipums_example("cps_00006.xml"),
      data_file = ipums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )

    cps_yield <- list()
    while(!cps_yield_source$is_done()) {
      cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
    }

    cps_chunked <- read_ipums_micro_chunked(
      ipums_example("cps_00006.xml"),
      IpumsListCallback$new(function(x, ...) x),
      1000,
      data_file = ipums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )

    expect_equal(cps_yield, cps_chunked)
  })

test_that(
  "basic yield reading - long .dat.gz hier matches chunked", {
    cps_yield_source <- read_ipums_micro_yield(
      ipums_example("cps_00010.xml"),
      data_file = ipums_example("cps_00010.dat.gz"),
      verbose = FALSE
    )

    cps_yield <- list()
    while(!cps_yield_source$is_done()) {
      cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
    }

    cps_chunked <- read_ipums_micro_chunked(
      ipums_example("cps_00010.xml"),
      IpumsListCallback$new(function(x, ...) x),
      1000,
      data_file = ipums_example("cps_00010.dat.gz"),
      verbose = FALSE
    )

    expect_equal(cps_yield, cps_chunked)
  })

test_that(
  "basic yield reading - list .dat.gz hier matches chunked", {
    cps_yield_source <- read_ipums_micro_list_yield(
      ipums_example("cps_00010.xml"),
      data_file = ipums_example("cps_00010.dat.gz"),
      verbose = FALSE
    )

    cps_yield <- list()
    while(!cps_yield_source$is_done()) {
      cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
    }

    cps_chunked <- read_ipums_micro_list_chunked(
      ipums_example("cps_00010.xml"),
      IpumsListCallback$new(function(x, ...) x),
      1000,
      data_file = ipums_example("cps_00010.dat.gz"),
      verbose = FALSE
    )

    expect_equal(cps_yield, cps_chunked)
  })

test_that(
  "basic yield reading - list .dat.gz rect matches chunked", {
    cps_yield_source <- read_ipums_micro_list_yield(
      ipums_example("cps_00006.xml"),
      data_file = ipums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )

    cps_yield <- list()
    while(!cps_yield_source$is_done()) {
      cps_yield[[length(cps_yield) + 1]] <- cps_yield_source$yield(1000)
    }

    cps_chunked <- read_ipums_micro_list_chunked(
      ipums_example("cps_00006.xml"),
      IpumsListCallback$new(function(x, ...) x),
      1000,
      data_file = ipums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )

    expect_equal(cps_yield, cps_chunked)
  })

test_that("read_ipums_micro_yield can't read from .csv file", {
  expect_error(
    cps_yield_source <- read_ipums_micro_yield(
      ddi = ipums_example("cps_00006.xml"),
      data_file = ipums_example("cps_00006.csv.gz"),
      verbose = FALSE
    ),
    regexp = "does not support"
  )
})
