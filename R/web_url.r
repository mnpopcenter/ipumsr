# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Launch a browser window to the ipums website
#'
#' Takes a DDI (or you can specify a project directly) and
#' a variable name, and makes a best guess at the URL for
#' the variable's page on the IPUMS website. Note that
#' NHGIS and TerraPop do not have accessible pages for
#' variables.
#'
#' Because some variables are constructed during the extract
#' creation process, the URL may not always work unfortunately.
#'
#'@param x A DDI or empty (if specifying project)
#'@param var A single variable name in a character vector
#'@param project If not using a DDI (or object with a project attribute)
#' A name of an IPUMS project, one of:
#'   "IPUMS-USA", "IPUMS-CPS", "IPUMS-International", "IPUMS-DHS",
#'   "ATUS-X", "AHTUS-X", "MTUS-X", "NHIS", "Higher Ed", "NHGIS",
#'   or "IPUMS Terra"
#' @param launch If \code{TRUE}, launch the website.
#' @param verbose If \code{TRUE}, message user if no variable specific websites are available
#' @param var_label Sometimes the variable label is useful for finding the correct URL. Only needed
#'   if not passing in the ddi object.
#' @param homepage_if_missing If \code{TRUE}, Return homepage if project does not provide variable
#'   specific web pages.
#' @return The url to the page on ipums.org (silently if launch is \code{TRUE})
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))
#' ipums_website(ddi, "MONTH", launch = FALSE)
#'
#' \dontrun{
#' # Launches website
#' ipums_website(ddi, "MONTH")
#' }
#'
#' # Can also specify project instead of using DDI
#' ipums_website(var = "RECTYPE", project = "IPUMS-CPS", launch = FALSE)
#'
#'
#'@export
ipums_website <- function(
  x, var, project = NULL, launch = TRUE, verbose = TRUE, var_label = NULL, homepage_if_missing = TRUE
) {
  UseMethod("ipums_website")
}

#'@export
ipums_website.ipums_ddi <- function(
  x, var, project = NULL, launch = TRUE, verbose = TRUE, var_label = NULL, homepage_if_missing = TRUE
) {
  if (is.null(project)) project <- x$ipums_project

  # Some convuluted code to check for "detailed variables", because their urls aren't right
  var <- fix_for_detailed_var(x, var, var_label)

  url <- get_ipums_url(var, project, verbose, homepage_if_missing)
  if (launch) {
    shell.exec(url)
    invisible(url)
  } else {
    url
  }
}

#'@export
ipums_website.default <- function(
  x, var, project = NULL, launch = TRUE, verbose = TRUE, var_label = NULL, homepage_if_missing = TRUE
) {
  if (is.null(project)) project <- attributes(x)[["ipums_project"]]
  if (missing(x)) x <- NULL

  # Some convuluted code to check for "detailed variables", because their urls aren't right
  var <- fix_for_detailed_var(x, var, var_label)

  url <- get_ipums_url(var, project, verbose, homepage_if_missing)
  if (launch) {
    shell.exec(url)
    invisible(url)
  } else {
    url
  }
}

get_ipums_url <- function(var, project, verbose = TRUE, homepage_if_missing = FALSE) {
  if (is.null(project)) {
    stop(paste(
      custom_format_text(
        "Project not found. Please specify the project name using ",
        "'project' argument. Options include: ", indent = 2, exdent = 2
      ),
      custom_format_text(
        paste(all_proj_names(), collapse = ", "), indent = 4, exdent = 4
      ),
      sep = "\n"
    ))
  }
  config <- get_proj_config(project)

  if (is.null(config)) {
    stop(paste(
      custom_format_text(
        "Unexpected project '", project, "'. ",
        "Options include: ", indent = 2, exdent = 2
      ),
      custom_format_text(
        paste(all_proj_names(), collapse = ", "), indent = 4, exdent = 4
      ),
      sep = "\n"
    ))
  }

  if (verbose && !config$var_url) {
    message("Cannot give a variable-specific URL for this project.")
  }

  if (!homepage_if_missing && !config$var_url) {
    return(NULL)
  }

  config$url_function(var)
}


# Some convuluted code to check for "detailed variables", because their urls aren't right
fix_for_detailed_var <- function(object, var, var_label) {
  if (is.null(var_label) & !is.null(object)) var_label <- ipums_var_label(object, one_of(var))

  if (is.null(var_label)) return(var)

  is_det <- grepl("detailed version", tolower(var_label), fixed = TRUE)

  if (is_det && fostr_sub(var, -1) == "D") {
    var <- fostr_sub(var, 1, -2)
  }
  var
}
