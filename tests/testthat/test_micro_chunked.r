context("Microdata - Chunked")
# B & list
# csv chunk reading is equivalent
# biglm is equivalent to lm

test_that(
  "basic chunk reading - long .dat.gz", {
    cps_full <- read_ipums_micro(
      ipums_example("cps_00006.xml"),
      data_file = ipums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )
    cps_full <- cps_full[cps_full$STATEFIP == 19, ]

    cps_chunked <- read_ipums_micro_chunked(
      ipums_example("cps_00006.xml"),
      IpumsDataFrameCallback$new(function(x, ...) x[x$STATEFIP == 19, ]),
      data_file = ipums_example("cps_00006.dat.gz"),
      verbose = FALSE
    )

    expect_equal(cps_full, cps_chunked)
  })

test_that(
  "basic chunk reading - long .csv.gz", {
    skip_on_cran() # Don't want to lock readr into hipread's callback API
    cps_full <- read_ipums_micro(
      ipums_example("cps_00006.xml"),
      data_file = ipums_example("cps_00006.csv.gz"),
      verbose = FALSE
    )
    cps_full <- cps_full[cps_full$STATEFIP == 19, ]

    cps_chunked <- read_ipums_micro_chunked(
      ipums_example("cps_00006.xml"),
      IpumsDataFrameCallback$new(function(x, ...) x[x$STATEFIP == 19, ]),
      data_file = ipums_example("cps_00006.csv.gz"),
      verbose = FALSE
    )

    expect_equal(cps_full, cps_chunked)
  })

test_that(
  "basic chunk reading - list", {
    cps_full <- read_ipums_micro_list(
      ipums_example("cps_00010.xml"),
      verbose = FALSE
    )

    cps_chunked <- read_ipums_micro_list_chunked(
      ipums_example("cps_00010.xml"),
      IpumsListCallback$new(function(x, ...) x),
      verbose = FALSE
    )

    cps_chunked_combined <- list(
      HOUSEHOLD = ipums_bind_rows(
        lapply(cps_chunked, function(x) x$HOUSEHOLD)
      ),
      PERSON = ipums_bind_rows(
        lapply(cps_chunked, function(x) x$PERSON)
      )
    )

    expect_equal(cps_full, cps_chunked_combined)
  })

test_that(
  "biglm is equivalent to lm", {
    biglm_results <- read_ipums_micro_chunked(
      ipums_example("cps_00015.xml"),
      IpumsBiglmCallback$new(
        INCTOT ~ AGE + HEALTH,
        function(x, ...) {
          x[x$INCTOT < 900000, ]
        }),
      chunk_size = 1000,
      var_attrs = NULL,
      verbose = FALSE
    )

    data <- read_ipums_micro(
      ipums_example("cps_00015.xml"),
      verbose = FALSE,
      var_attrs = NULL
    )
    data <- data[data$INCTOT < 900000, ]

    lm_results <- lm(INCTOT ~ AGE + HEALTH, data)

    expect_equal(coef(biglm_results), coef(lm_results))
    expect_equal(vcov(biglm_results), vcov(lm_results))
  }
)
