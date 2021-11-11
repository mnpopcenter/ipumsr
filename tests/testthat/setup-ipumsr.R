library("vcr") # *Required* as vcr is set up on loading

vcr_dir <- vcr::vcr_test_path("fixtures")

have_api_access <- TRUE

if (!nzchar(Sys.getenv("IPUMS_API_KEY"))) {
  if (dir.exists(vcr_dir) && length(dir(vcr_dir)) > 0) {
    # Fake API token to fool ipumsr API functions
    Sys.setenv("IPUMS_API_KEY" = "foobar")
  } else {
    # If there are no mock files nor API token, can't run API tests
    have_api_access <- FALSE
  }
}

invisible(vcr::vcr_configure(
  filter_sensitive_data = list(
    "<<<IPUMS_API_KEY>>>" = Sys.getenv("IPUMS_API_KEY")
  ),
  write_disk_path = vcr_dir,
  dir = vcr_dir
))
vcr::check_cassette_names()
