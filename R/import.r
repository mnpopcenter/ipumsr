# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums


# Import all of rlang
#'@import rlang
NULL

# Import and reexport helpful label functions from haven
#' @importFrom haven as_factor
#' @export
haven::as_factor

#' @importFrom haven zap_labels
#' @export
haven::zap_labels

#' @importFrom haven is.labelled
#' @export
haven::is.labelled

# ---- Select Helpers ----

#' Select-style helpers from dplyr
#'
#' Several arguments in \code{ripums} allow syntax for selecting variables
#' based on dplyr's \code{\link[dplyr]{select}} function. See details for more information.
#'
#' There are 3 broad categories of methods for specifying arguments for these select-style
#' parameters.
#' \itemize{
#'  \item{"Character Vector"}{A character vector of names (such as \code{c("var1", "var2", "var3")})}
#'  \item{"'Bare' Vector"}{A vector of 'bare' names (such as \code{c(var1, var2, var3)})}
#'  \item{"Helper Functions"}{Helper functions from \code{dplyr::select} such as
#'     \code{starts_with()}, \code{contains} and others.}
#' }
#' @examples
#' \dontrun{
#' # For microdata, use it to load variables
#' # Load 3 variables by name
#' data <- read_ipums_micro("cps_00001.xml", vars = c("RECTYPE", "YEAR", "TCIG100"))
#'
#' # Load same 3 variables using bare names
#' data <- read_ipums_micro("cps_00001.xml", vars = c(RECTYPE, YEAR, TCIG100))
#'
#' # Use helper functions to load all variables that start with REPWT
#' data <- read_ipums_micro("cps_00001.xml", vars = starts_with("REPWT"))
#'
#' # Use bare names and helper function to load RECTYPE, YEAR and all variables with 'CIG' in name
#' data <- read_ipums_micro("cps_00001.xml", vars = c(RECTYPE, YEAR, contains("CIG")))
#'
#' # For geographic extracts, `data_layer` and `shape_layer` arguments use the same conventions
#' # to select file names from within zip files.
#' data <- read_nhgis(
#'   "nhgis0001_csv.zip",
#'   "nhgis0001_shape.zip",
#'   data_layer = contains("state")
#' )
#'
#' }
#' @name dplyr_select_style
NULL

#' @importFrom dplyr starts_with
#' @export
dplyr::starts_with

#' @importFrom dplyr ends_with
#' @export
dplyr::ends_with

#' @importFrom dplyr contains
#' @export
dplyr::contains

#' @importFrom dplyr matches
#' @export
dplyr::matches

#' @importFrom dplyr num_range
#' @export
dplyr::num_range

#' @importFrom dplyr starts_with
#' @export
dplyr::starts_with

#' @importFrom dplyr one_of
#' @export
dplyr::one_of

#' @importFrom dplyr everything
#' @export
dplyr::everything


