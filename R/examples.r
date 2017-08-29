# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' Get path to ripums example datasets
#'
#' Get access to example extracts. Some extracts (such as the full
#' nhgis shape file and vignette examples) are too big for inclusion
#' on CRAN and so have been included in a spearate package called
#' 'ripumsexamples'.
#'
#' The 'ripumsexamples' package can be installed using the command:
#' \code{devtools::install_github('mnpopcenter/ripums', subdir = "ripumsexamples")}
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' ripums_example()
#' ripums_example("cps_00006.xml")
ripums_example <- function(path = NULL) {
  if (is.null(path)) {
    file <- dir(system.file("extdata", package = "ripums"))
    if (!requireNamespace("ripumsexamples", quietly = TRUE)) {
      warning(paste0(
        "Some IPUMS example extracts are found in the 'ripumsexamples' package, which ",
        "can be installed using the command: ",
        "devtools::install_github('mnpopcenter/ripums', subdir = 'ripumsexamples')"
      ))
    } else {
      file <- c(file, dir(system.file("extdata", package = "ripumsexamples")))
    }


  } else {
    file <- system.file("extdata", path, package = "ripums")
    if (!file.exists(file)) {
      if (requireNamespace("ripumsexamples", quietly = TRUE)) {
        file <- system.file("extdata", path, package = "ripumsexamples", mustWork = TRUE)
      } else {
        stop(paste0(
          "Some IPUMS example extracts are found in the 'ripumsexample' package, which ",
          "can be installed using the command: ",
          "devtools::install_github('mnpopcenter/ripums', subdir = 'ripumsexamples')"
        ))
      }
    }
  }
  file
}
