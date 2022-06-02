modify_ready_extract_cassette_file <- function(cassette_file_name) {
  ready_extract_cassette_file <- file.path(
    vcr::vcr_test_path("fixtures"), cassette_file_name
  )

  ready_lines <- readLines(ready_extract_cassette_file)
  last_request_start_line <- max(which(grepl("^- request:", ready_lines)))
  writeLines(
    c(
      ready_lines[[1]],
      ready_lines[last_request_start_line:length(ready_lines)]
    ),
    con = ready_extract_cassette_file
  )
}
