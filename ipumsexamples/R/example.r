# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Get path to ipumsr's extra example datasets
#'
#' Get access to example extracts.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @return The filepath to an example file, or if path is empty, a vector of all
#'   available files.
#' @export
#' @examples
#' ipums_extra_example() # Lists all available examples
#' ipums_extra_example("nhgis0010_csv.zip") # Gives filepath for a nhgis shape
ipums_extra_example <- function(path = NULL) {
  if (is.null(path)) {
    file <- dir(system.file("extdata", package = "ipumsexamples"))
  } else {
    file <- system.file("extdata", path, package = "ipumsexamples")
    if (!file.exists(file)) {
      all_files <- paste(dir(system.file("extdata", package = "ipumsexamples")), collapse = ", ")
      stop(paste0(
        "Could not find file '", path, "' in extra examples. Available files are:\n",
        all_files
      ))
    }
  }
  file
}
