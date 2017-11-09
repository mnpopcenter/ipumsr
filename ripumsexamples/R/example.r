# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' Get path to ripums example datasets
#'
#' Get access to example extracts.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @return The filepath to an example file, or if path is empty, a vector of all
#'   available files.
#' @export
#' @examples
#' ripums_extra_example() # Lists all available examples
#' ripums_extra_example("nhgis0010_csv.zip") # Gives filepath for a nhgis shape
ripums_extra_example <- function(path = NULL) {
  if (is.null(path)) {
    file <- dir(system.file("extdata", package = "ripumsexamples"))
  } else {
    file <- system.file("extdata", path, package = "ripumsexamples")
    if (!file.exists(file)) {
      all_files <- paste(dir(system.file("extdata", package = "ripumsexamples")), collapse = ", ")
      stop(paste0(
        "Could not find file '", path, "' in extra examples. Available files are:\n",
        all_files
      ))
    }
  }
  file
}
