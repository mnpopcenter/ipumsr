library(dplyr)
library(purrr)

# Setup ----
usa_extract <- define_extract(
  collection = "usa",
  samples = "us2017b",
  variables = "YEAR",
  description = "Test extract",
  data_format = "fixed_width"
)

if (have_api_access) {
  vcr::use_cassette("submitted-usa-extract", {
    submitted_usa_extract <- submit_extract(usa_extract)
  })

  vcr::use_cassette("ready-usa-extract", {
    ready_usa_extract <- wait_for_extract(submitted_usa_extract)
  })

  # Modify ready-usa-extract.yml to only includes the final http request, so that
  # it returns the ready-to-download extract immediately on subsequent runs
  ready_usa_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "ready-usa-extract.yml"
  )

  ready_usa_lines <- readLines(ready_usa_extract_cassette_file)
  last_request_start_line <- max(which(grepl("^- request:", ready_usa_lines)))
  writeLines(
    c(
      ready_usa_lines[[1]],
      ready_usa_lines[last_request_start_line:length(ready_usa_lines)]
    ),
    con = ready_usa_extract_cassette_file
  )


  vcr::use_cassette("recent-usa-extracts-list", {
    recent_usa_extracts_list <- get_recent_extracts_info_list("usa")
  })

  vcr::use_cassette("recent-usa-extracts-tbl", {
    recent_usa_extracts_tbl <- get_recent_extracts_info_tbl("usa")
  })
}


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
  skip_if_no_api_access(have_api_access)
  expect_s3_class(submitted_usa_extract, "ipums_extract")
  expect_equal(submitted_usa_extract$collection, "usa")
  expect_true(submitted_usa_extract$submitted)
  expect_equal(submitted_usa_extract$status, "queued")
  expect_identical(
    submitted_usa_extract$download_links,
    ipumsr:::EMPTY_NAMED_LIST
  )
})


test_that("ipums_extract print method works", {
  expect_output(print(usa_extract), regexp = "Unsubmitted IPUMS USA extract")
})


test_that("Can check the status of a USA extract by supplying extract object", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(submitted_usa_extract)
  })
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-usa-extract-ready", {
    is_ready <- is_extract_ready(submitted_usa_extract)
  })
  expect_true(is_ready)
})


test_that("Can check the status of a USA extract by supplying collection and number", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(c("usa", submitted_usa_extract$number))
  })
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-usa-extract-ready", {
    is_ready <- is_extract_ready(c("usa", submitted_usa_extract$number))
  })
  expect_true(is_ready)
  vcr::use_cassette("get-usa-extract-info", {
    extract <- get_extract_info(paste0("usa:", submitted_usa_extract$number))
  })
  expect_s3_class(extract, "ipums_extract")
  expect_true(extract$status == "completed")
  vcr::use_cassette("is-usa-extract-ready", {
    is_ready <- is_extract_ready(paste0("usa:", submitted_usa_extract$number))
  })
  expect_true(is_ready)
})


test_that("Tibble of recent USA extracts contains expected columns", {
  skip_if_no_api_access(have_api_access)
  expected_columns <- c("collection", "description", "data_structure",
                        "rectangular_on", "data_format", "samples", "variables",
                        "submitted", "download_links", "number", "status")
  expect_setequal(names(recent_usa_extracts_tbl), expected_columns)
})


test_that("Can limit number of recent extracts to get info on", {
  skip_if_no_api_access(have_api_access)
  vcr::use_cassette("recent-usa-extracts-tbl-two", {
    two_recent_usa_extracts <- get_recent_extracts_info_tbl("usa", 2)
  })
  expect_equal(nrow(two_recent_usa_extracts), 2)
})


if (have_api_access) {
  download_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "download-usa-extract-ipums-extract.yml"
  )

  already_existed <- file.exists(download_extract_cassette_file)
}


download_dir <- file.path(tempdir(), "ipums-api-downloads")
if (!dir.exists(download_dir)) dir.create(download_dir)

tryCatch(
  vcr::use_cassette("download-usa-extract-ipums-extract", {
    test_that("Can download a USA extract by supplying extract object", {
      skip_if_no_api_access(have_api_access)
      expect_message(
        ddi_file_path <- download_extract(
          ready_usa_extract,
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "DDI codebook file saved to"
      )
      ddi_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(ddi_file_path)
      )
      ddi_file_path <- convert_to_relative_path(ddi_file_path)
      expect_match(ddi_file_path, "\\.xml$")
      expect_true(file.exists(ddi_file_path))
      data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
      expect_equal(nrow(data), 20972)
    })
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)


if (have_api_access) {
  if (!already_existed) {
    convert_paths_in_cassette_file_to_relative(download_extract_cassette_file)
  }

  download_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), "download-usa-extract-collection-number.yml"
  )

  already_existed <- file.exists(download_extract_cassette_file)
}

tryCatch(
  vcr::use_cassette("download-usa-extract-collection-number", {
    skip_if_no_api_access(have_api_access)
    test_that("Can download a USA extract by supplying collection and number as vector", {
      expect_message(
        ddi_file_path <- download_extract(
          c("usa", ready_usa_extract$number),
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "DDI codebook file saved to"
      )
      ddi_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(ddi_file_path)
      )
      ddi_file_path <- convert_to_relative_path(ddi_file_path)
      expect_match(ddi_file_path, "\\.xml$")
      expect_true(file.exists(ddi_file_path))
      data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
      expect_equal(nrow(data), 20972)
    })
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)

if (have_api_access) {
  if (!already_existed) {
    convert_paths_in_cassette_file_to_relative(download_extract_cassette_file)
  }
}

tryCatch(
  vcr::use_cassette("download-usa-extract-collection-number", {
    skip_if_no_api_access(have_api_access)
    test_that("Can download a USA extract by supplying collection and number as string", {
      expect_message(
        ddi_file_path <- download_extract(
          paste0("usa:", ready_usa_extract$number),
          download_dir = download_dir,
          overwrite = TRUE
        ),
        regexp = "DDI codebook file saved to"
      )
      ddi_file_path <- file.path(
        vcr::vcr_test_path("fixtures"),
        basename(ddi_file_path)
      )
      ddi_file_path <- convert_to_relative_path(ddi_file_path)
      expect_match(ddi_file_path, "\\.xml$")
      expect_true(file.exists(ddi_file_path))
      data <- read_ipums_micro(ddi_file_path, verbose = FALSE)
      expect_equal(nrow(data), 20972)
    })
  }),
  warning = function(w) {
    if (!grepl("Empty cassette", w$message)) {
      return(warning(w$message, call. = FALSE))
    }
  }
)


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
    usa_extract,
    samples_to_add = "us2014a",
    vars_to_add = "RELATE"
  )
  expect_true(revised_extract$status == "unsubmitted")
  expect_equal(
    revised_extract$description,
    paste0("Revision of (", usa_extract$description, ")")
  )
  expect_equal(
    revised_extract$samples,
    c(usa_extract$samples, "us2014a")
  )
  expect_equal(
    revised_extract$variables,
    c(usa_extract$variables, "RELATE")
  )
})

test_that("Can revise a submitted extract", {
  skip_if_no_api_access(have_api_access)
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
    revise_extract(usa_extract, samples_to_add = "us2017b"),
    regexp = "already included"
  )
  expect_warning(
    revise_extract(usa_extract, vars_to_remove = "RELATE"),
    regexp = "are not included"
  )
})

test_that("tbl to list and list to tbl conversion works", {
  skip_if_no_api_access(have_api_access)
  converted_to_list <- extract_tbl_to_list(
    recent_usa_extracts_tbl,
    validate = FALSE
  )
  converted_to_tbl <- extract_list_to_tbl(recent_usa_extracts_list)
  expect_identical(recent_usa_extracts_list, converted_to_list)
  expect_identical(recent_usa_extracts_tbl, converted_to_tbl)
})

test_that("We can export to and import from JSON", {
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile))
  save_extract_as_json(usa_extract, json_tmpfile)
  copy_of_usa_extract <- define_extract_from_json(json_tmpfile, "usa")
  expect_identical(usa_extract, copy_of_usa_extract)
})

test_that("We can export to and import from JSON, submitted extract", {
  skip_if_no_api_access(have_api_access)
  json_tmpfile <- file.path(tempdir(), "usa_extract.json")
  on.exit(unlink(json_tmpfile))
  save_extract_as_json(submitted_usa_extract, json_tmpfile)
  copy_of_submitted_usa_extract <- define_extract_from_json(json_tmpfile, "usa")
  expect_identical(
    ipumsr:::copy_ipums_extract(submitted_usa_extract),
    copy_of_submitted_usa_extract
  )
})
