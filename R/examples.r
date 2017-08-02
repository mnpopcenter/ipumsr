# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport

#' Get path to ipumsimport example
#'
#' Get access to example extracts from this package's `inst/extdata`
#' directory.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' ipumsimport_example()
#' ipumsimport_example("cps00006.xml")
ipumsimport_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "ipumsimport"))
  } else {
    system.file("extdata", path, package = "ipumsimport", mustWork = TRUE)
  }
}
