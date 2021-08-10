library(dplyr)
library(purrr)

# Setup ----
usa_extract <- ipums_api_build_extract(
  collection = "usa",
  samples = "us2013a",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

submitted_usa_extract <- ipums_api_submit_extract(usa_extract)

cps_extract <- ipums_api_build_extract(
  collection = "cps",
  samples = c("cps1976_01s", "cps1976_02b"),
  variables = c("YEAR", "MISH", "CPSIDP", "AGE", "SEX", "RACE", "UH_SEX_B1"),
  description = "Compare age-sex-race breakdowns 1976",
  data_format = "fixed_width"
)

submitted_cps_extract <- ipums_api_submit_extract(cps_extract)

ipumsi_extract <- ipums_api_build_extract(
  collection = "ipumsi",
  samples = "lc1980a",
  variables = c("AGE", "SEX"),
  description = "Family structure Saint Lucia 1980",
  data_format = "fixed_width"
)

submitted_ipumsi_extract <- ipums_api_submit_extract(ipumsi_extract)

recent_usa_extracts <- ipums_api_get_recent_extracts_tbl("usa")
is_downloadable_usa <- ipums_api_extract_downloadable(recent_usa_extracts)
skip_usa_download <- !any(is_downloadable_usa)
if (!skip_usa_download) {
  downloadable_usa_extract <- recent_usa_extracts %>%
    filter(is_downloadable_usa) %>%
    slice(1) %>%
    ipumsr:::extract_tbl_to_list() %>%
    .[[1]]
}


recent_cps_extracts <- ipums_api_get_recent_extracts_tbl("cps")
is_downloadable_cps <- ipums_api_extract_downloadable(recent_cps_extracts)
skip_cps_download <- !any(is_downloadable_cps)
if (!skip_cps_download) {
  downloadable_cps_extract <- recent_cps_extracts %>%
    filter(is_downloadable_cps) %>%
    slice(1) %>%
    ipumsr:::extract_tbl_to_list() %>%
    .[[1]]
}


recent_ipumsi_extracts <- ipums_api_get_recent_extracts_tbl("ipumsi")
is_downloadable_ipumsi <- ipums_api_extract_downloadable(recent_ipumsi_extracts)
skip_ipumsi_download <- !any(is_downloadable_ipumsi)
if (!skip_ipumsi_download) {
  downloadable_ipumsi_extract <- recent_ipumsi_extracts %>%
    filter(is_downloadable_ipumsi) %>%
    slice(1) %>%
    ipumsr:::extract_tbl_to_list() %>%
    .[[1]]
}


# Tests ----
test_that("Can define an extract", {
  expect_s3_class(usa_extract, "ipums_extract")
  expect_equal(usa_extract$variables[[1]], "YEAR")
  expect_equal(usa_extract$data_structure, "rectangular")
  expect_equal(usa_extract$rectangular_on, "P")
  expect_identical(usa_extract$download_links, ipumsr:::empty_named_list())
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
    ipumsr:::empty_named_list()
  )
})


test_that("Can submit a CPS extract", {
  expect_s3_class(submitted_cps_extract, "ipums_extract")
  expect_equal(submitted_cps_extract$collection, "cps")
  expect_true(submitted_cps_extract$submitted)
  expect_equal(submitted_cps_extract$status, "queued")
  expect_identical(submitted_cps_extract$download_links, ipumsr:::empty_named_list())
})


test_that("ipums_extract print method works", {
  expect_output(print(cps_extract), regexp = "Unsubmitted IPUMS CPS extract")
})


test_that("Can check the status of a USA extract by supplying extract object", {
  extract <- ipums_api_get_extract(submitted_usa_extract)
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Can check the status of a CPS extract by supplying extract object", {
  extract <- ipums_api_get_extract(submitted_cps_extract)
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Can check the status of a USA extract by supplying collection and number", {
  extract <- ipums_api_get_extract("usa", 1)
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Can check the status of a CPS extract by supplying collection and number", {
  extract <- ipums_api_get_extract("cps", 1)
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status %in% c("queued", "started", "completed"))
})


test_that("Tibble of recent USA extracts contains expected columns", {
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_usa_extracts), expected_columns)
})


test_that("Can limit number of recent extracts to get info on", {
  two_recent_usa_extracts <- ipums_api_get_recent_extracts_tbl(
    "usa",
    how_many = 2
  )
  expect_equal(nrow(two_recent_usa_extracts), 2)
})


test_that("Tibble of recent CPS extracts contains expected columns", {
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_cps_extracts), expected_columns)
})

download_dir <- file.path(tempdir(), "ipums-api-downloads")
if (!dir.exists(download_dir)) dir.create(download_dir)

test_that("Can download a USA extract by supplying extract object", {
  skip_if_not(exists("downloadable_usa_extract"))
  expect_message(
    ddi_file_path <- ipums_api_download_extract(
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
  skip_if_not(exists("downloadable_usa_extract"))
  expect_message(
    ddi_file_path <- ipums_api_download_extract(
      collection = downloadable_usa_extract$collection,
      extract_number = downloadable_usa_extract$number,
      download_dir = download_dir,
      overwrite = TRUE
    ),
    regexp = "DDI codebook file saved to"
  )
  expect_match(ddi_file_path, "\\.xml$")
  expect_true(file.exists(ddi_file_path))
})


test_that("Can download a CPS extract by supplying extract object", {
  skip_if_not(exists("downloadable_cps_extract"))
  expect_message(
    ddi_file_path <- ipums_api_download_extract(
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
  skip_if_not(exists("downloadable_cps_extract"))
  expect_message(
    ddi_file_path <- ipums_api_download_extract(
      collection = downloadable_cps_extract$collection,
      extract_number = downloadable_cps_extract$number,
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
    ipums_api_submit_extract(ipumsr:::new_ipums_extract()),
    regexp = paste0(
      "The following elements of an ipums_extract must not contain missing ",
      "values:"
    )
  )
})

test_that("An extract request with missing samples returns correct error", {
  expect_error(
    ipums_api_submit_extract(ipumsr:::new_ipums_extract(collection = "usa")),
    regexp = paste0(
      "The following elements of an ipums_extract must not contain missing ",
      "values:"
    )
  )
})

test_that("An extract request with missing samples returns correct error", {
  expect_error(
    ipums_api_submit_extract(
      ipumsr:::new_ipums_extract(collection = "usa", description = "Test")
    ),
    regexp = paste0(
      "The following elements of an ipums_extract must not contain missing ",
      "values:"
    )
  )
})

