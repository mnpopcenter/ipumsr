# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

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
#'@export
ipums_website <- function(x, var, project = NULL, launch = TRUE, verbose = TRUE) {
  UseMethod("ipums_website")
}

#'@export
ipums_website.ipums_ddi <- function(x, var, project = NULL, launch = TRUE, verbose = TRUE) {
  if (is.null(project)) project <- x$ipums_project
  url <- get_ipums_url(var, project)
  if (launch) {
    shell.exec(url)
    invisible(url)
  } else {
    url
  }
}

#'@export
ipums_website.default <- function(x, var, project = NULL, launch = TRUE, verbose = TRUE) {
  if (is.null(project)) project <- attributes(x)[["ipums_project"]]
  url <- get_ipums_url(var, project)
  if (launch) {
    shell.exec(url)
    invisible(url)
  } else {
    url
  }
}

get_ipums_url <- function(var, project, verbose = TRUE) {
  if (is.null(project)) {
    stop(paste0("Project not found. Please specify the project name using 'project' argument. ",
                "Options include: ", paste(all_proj_names(), collapse = ", ")
    ))
  }
  config <- get_proj_config(project)

  if (is.null(config)) {
    stop(paste0(
      "Unexpected project '", project, "'. ",
      "Options include: ", paste(all_proj_names(), collapse = ", ")
    ))
  }

  if (verbose && !config$var_url) {
    message("Cannot give a variable-specific URL for this project.")
  }

  config$url_function(var)
}
