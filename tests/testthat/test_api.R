library(dplyr)
library(purrr)

# Setup ----
usa_extract <- define_extract(
  collection = "usa",
  samples = "us2013a",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

submitted_usa_extract <- submit_extract(usa_extract)

cps_extract <- define_extract(
  collection = "cps",
  samples = c("cps1976_01s", "cps1976_02b"),
  variables = c("YEAR", "MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1"),
  description = "Compare age-sex-race breakdowns 1976",
  data_format = "fixed_width"
)

submitted_cps_extract <- submit_extract(cps_extract)

ipumsi_extract <- define_extract(
  collection = "ipumsi",
  samples = "lc1980a",
  variables = c("AGE", "SEX"),
  description = "Family structure Saint Lucia 1980",
  data_format = "fixed_width"
)

submitted_ipumsi_extract <- submit_extract(ipumsi_extract)

recent_usa_extracts <- get_recent_extracts_info_list("usa")
downloadable_usa_extract <- recent_usa_extracts %>%
  keep(is_extract_ready) %>%
  pluck(1)

skip_usa_download <- is.null(downloadable_usa_extract)

if (!skip_usa_download) {
  downloadable_usa_extract <- recent_usa_extracts %>%
    keep(is_extract_ready) %>%
    pluck(1)
}


recent_cps_extracts <- get_recent_extracts_info_list("cps")
downloadable_cps_extract <- recent_cps_extracts %>%
  keep(is_extract_ready) %>%
  pluck(1)
skip_cps_download <- is.null(downloadable_cps_extract)


recent_ipumsi_extracts <- get_recent_extracts_info_list("ipumsi")
downloadable_ipumsi_extract <- recent_ipumsi_extracts %>%
  keep(is_extract_ready) %>%
  pluck(1)
skip_ipumsi_download <- is.null(downloadable_ipumsi_extract)


# Tests ----
test_that("Can define an extract", {
  expect_s3_class(usa_extract, "ipums_extract")
  expect_equal(usa_extract$variables[[1]], "YEAR")
  expect_equal(usa_extract$data_structure, "rectangular")
  expect_equal(usa_extract$rectangular_on, "P")
  expect_identical(usa_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
  expect_false(usa_extract$submitted)
  expect_equal(usa_extract$number, NA_integer_)
  expect_equal(usa_extract$status, "unsubmitted")
})


test_that("Can submit a USA extract", {
  expect_s3_class(submitted_usa_extract, "ipums_extract")
  expect_equal(submitted_usa_extract$collection, "usa")
  expect_true(submitted_usa_extract$submitted)
  expect_equal(submitted_usa_extract$status, "queued")
  expect_identical(
    submitted_usa_extract$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
})


test_that("Can submit a CPS extract", {
  expect_s3_class(submitted_cps_extract, "ipums_extract")
  expect_equal(submitted_cps_extract$collection, "cps")
  expect_true(submitted_cps_extract$submitted)
  expect_equal(submitted_cps_extract$status, "queued")
  expect_identical(submitted_cps_extract$download_links, ipumsr:::EMPTY_NAMED_LIST)
})


test_that("ipums_extract print method works", {
  expect_output(print(cps_extract), regexp = "Unsubmitted IPUMS CPS extract")
})


test_that("Can check the status of a USA extract by supplying extract object", {
  extract <- get_extract_info(submitted_usa_extract)
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Can check the status of a CPS extract by supplying extract object", {
  extract <- get_extract_info(submitted_cps_extract)
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Can check the status of a USA extract by supplying collection and number", {
  extract <- get_extract_info(c("usa", "1"))
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Can check the status of a CPS extract by supplying collection and number", {
  extract <- get_extract_info("cps:1")
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Tibble of recent USA extracts contains expected columns", {
  recent_usa_extracts_tbl <- get_recent_extracts_info_tbl("usa")
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_usa_extracts_tbl), expected_columns)
})


test_that("Can limit number of recent extracts to get info on", {
  two_recent_usa_extracts <- get_recent_extracts_info_tbl(
    "usa",
    how_many = 2
  )
  expect_equal(nrow(two_recent_usa_extracts), 2)
})


test_that("Tibble of recent CPS extracts contains expected columns", {
  recent_cps_extracts_tbl <- get_recent_extracts_info_tbl("cps")
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_cps_extracts_tbl), expected_columns)
})

download_dir <- file.path(tempdir(), "ipums-api-downloads")
if (!dir.exists(download_dir)) dir.create(download_dir)

test_that("Can download a USA extract by supplying extract object", {
  skip_if(skip_usa_download)
  expect_message(
    ddi_file_path <- download_extract(
      downloadable_usa_extract,
      download_dir = download_dir,
      overwrite = TRUE
    ),
    regexp = "DDI codebook file saved to"
  )
  expect_match(ddi_file_path, "\\.xml$")
  expect_true(file.exists(ddi_file_path))
})


test_that("Can download a USA extract by supplying collection and number", {
  skip_if(skip_usa_download)
  expect_message(
    ddi_file_path <- download_extract(
      c(downloadable_usa_extract$collection, downloadable_usa_extract$number),
      download_dir = download_dir,
      overwrite = TRUE
    ),
    regexp = "DDI codebook file saved to"
  )
  expect_match(ddi_file_path, "\\.xml$")
  expect_true(file.exists(ddi_file_path))
})


test_that("Can download a CPS extract by supplying extract object", {
  skip_if(skip_cps_download)
  expect_message(
    ddi_file_path <- download_extract(
      downloadable_cps_extract,
      download_dir = download_dir,
      overwrite = TRUE
    ),
    regexp = "DDI codebook file saved to"
  )
  expect_match(ddi_file_path, "\\.xml$")
  expect_true(file.exists(ddi_file_path))
  ddi <- read_ipums_ddi(ddi_file_path)
  data <- read_ipums_micro(ddi)
  expect_s3_class(data, "data.frame")
})


test_that("Can download a CPS extract by supplying collection and number", {
  skip_if(skip_cps_download)
  expect_message(
    ddi_file_path <- download_extract(
      paste0(
        downloadable_cps_extract$collection, ":",
        downloadable_cps_extract$number
      ),
      download_dir = download_dir,
      overwrite = TRUE
    ),
    regexp = "DDI codebook file saved to"
  )
  expect_match(ddi_file_path, "\\.xml$")
  expect_true(file.exists(ddi_file_path))
})


test_that("An extract request with missing collection returns correct error", {
  expect_error(
    submit_extract(ipumsr:::new_ipums_extract()),
    regexp = paste0(
      "The following elements of an ipums_extract must not contain missing ",
      "values:"
    )
  )
})

test_that("An extract request with missing samples returns correct error", {
  expect_error(
    submit_extract(ipumsr:::new_ipums_extract(collection = "usa")),
    regexp = paste0(
      "The following elements of an ipums_extract must not contain missing ",
      "values:"
    )
  )
})

test_that("An extract request with missing samples returns correct error", {
  expect_error(
    submit_extract(
      ipumsr:::new_ipums_extract(collection = "usa", description = "Test")
    ),
    regexp = paste0(
      "The following elements of an ipums_extract must not contain missing ",
      "values:"
    )
  )
})

test_that("Can revise an extract", {
  revised_extract <- revise_extract(
    submitted_usa_extract,
    samples_to_add = "us2014a",
    vars_to_add = "RELATE"
  )
  expect_true(revised_extract$status == "unsubmitted")
  expect_equal(
    revised_extract$description,
    paste0("Revision of (", submitted_usa_extract$description, ")")
  )
  expect_equal(
    revised_extract$samples,
    c(submitted_usa_extract$samples, "us2014a")
  )
  expect_equal(
    revised_extract$variables,
    c(submitted_usa_extract$variables, "RELATE")
  )
})

test_that("We warn user when their revisions don't make sense", {
  expect_warning(
    revise_extract(submitted_usa_extract, samples_to_add = "us2013a"),
    regexp = "already included"
  )
  expect_warning(
    revise_extract(submitted_usa_extract, vars_to_remove = "RELATE"),
    regexp = "are not included"
  )
})

test_that("tbl to list and list to tbl conversion works", {
  recent_list <- get_recent_extracts_info_list("usa")
  recent_tbl <- get_recent_extracts_info_tbl("usa")
  converted_to_list <- extract_tbl_to_list(recent_tbl, validate = FALSE)
  converted_to_tbl <- extract_list_to_tbl(recent_list)
  expect_identical(recent_list, converted_to_list)
  expect_identical(recent_tbl, converted_to_tbl)
})

test_that("We can export to and import from JSON", {
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile))
  save_extract_as_json(usa_extract, json_tmpfile)
  copy_of_usa_extract <- define_extract_from_json(json_tmpfile, "usa")
  expect_identical(usa_extract, copy_of_usa_extract)
  save_extract_as_json(submitted_usa_extract, json_tmpfile)
  copy_of_submitted_usa_extract <- define_extract_from_json(json_tmpfile, "usa")
  expect_identical(submitted_usa_extract, copy_of_submitted_usa_extract)
})
