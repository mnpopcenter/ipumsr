# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' Get path to ripumstest examples
#'
#' Get access to example extracts from the ripumstest `inst/extdata`
#' directory. This data has been put in a separate file because they
#' are too large for CRAN.
#'
#' The 'ripumstest' package can be installed using the command:
#' \code{devtools::install_github('mnpopcenter/ripumstest')}
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' ripums_example()
#' ripums_example("cps_00006.xml")
ripums_example <- function(path = NULL) {
  if (!requireNamespace("ripumstest", quietly = TRUE)) {
    stop(paste0(
      "IPUMS example extracts are found in the 'ripumstest' package, which ",
      "can be installed using the command: devtools::install_github('mnpopcenter/ripumstest')."
    ))
  }

  if (is.null(path)) {
    dir(system.file("extdata", package = "ripumstest"))
  } else {
    system.file("extdata", path, package = "ripumstest", mustWork = TRUE)
  }
}
