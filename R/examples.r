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
#' ripums_example() # Lists all available examples
#' ripums_example("cps_00006.xml") # Gives filepath for a cps DDI
ripums_example <- function(path = NULL) {
  if (is.null(path)) {
    file <- dir(system.file("extdata", package = "ripums"))
  } else {
    file <- system.file("extdata", path, package = "ripums")
    if (!file.exists(file)) {
      all_files <- paste(dir(system.file("extdata", package = "ripums")), collapse = ", ")
      stop(paste(
        custom_format_text(
          "Could not find file '", path, "' in examples. Available files are:",
          indent = 2, exdent = 2
        ),
        custom_format_text(all_files, indent = 4, exdent = 4),
        sep = "\n"
      ))
    }
  }
  file
}
