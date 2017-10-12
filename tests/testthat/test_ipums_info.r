context("IPUMS Info")

nvars <- 8
first_three_vars <- c("YEAR", "SERIAL", "HWTSUPP")
year_label <- "Survey year"
year_var_desc <- "YEAR reports the year in which the survey was conducted.  YEARP is repeated on person records."

file_info_types <- c("ipums_project", "extract_date", "extract_notes", "conditions", "citation")

test_that("Var info/var desc/var label/value labels works on DDI", {
  ddi <- read_ipums_ddi(ripums_example("cps_00006.xml"))

  expect_equal(nrow(ipums_var_info(ddi)), nvars)
  expect_equal(ipums_var_info(ddi)$var_name[1:3], first_three_vars)

  expect_true(identical(ipums_var_info(ddi)[1, ], ipums_var_info(ddi, "YEAR")))

  expect_equal(ipums_var_label(ddi, "YEAR"), year_label)

  expect_equal(ipums_var_desc(ddi, "YEAR"), year_var_desc)
})

test_that("Var info/var desc/var label/value labels works on data", {
  data <- read_ipums_micro(ripums_example("cps_00006.xml"), verbose = FALSE)

  expect_equal(nrow(ipums_var_info(data)), nvars)
  expect_equal(ipums_var_info(data)$var_name[1:3], first_three_vars)

  expect_true(identical(ipums_var_info(data)[1, ], ipums_var_info(data, "YEAR")))

  expect_equal(ipums_var_label(data, "YEAR"), year_label)

  expect_equal(ipums_var_desc(data, "YEAR"), year_var_desc)
})

test_that("File info works on ddi", {
  ddi <- read_ipums_ddi(ripums_example("cps_00006.xml"))

  expect_equal(names(ipums_file_info(ddi)), file_info_types)
  expect_equal(ipums_file_info(ddi, "ipums_project"), "IPUMS-CPS")
})

