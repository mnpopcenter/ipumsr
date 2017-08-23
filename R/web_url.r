# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' Launch a browser window to the ipums website
#'
#' Takes a DDI (or other object with ipums metadata) and
#' a variable name, and launches a web browser to the variable's
#' page on the ipums.org website. You can also specify a project.
#'
#'@param x A DDI or empty (if specifying project)
#'@param var A single variable name in a character vector
#'@param prjoect If not using a DDI (or object with a project attribute)
#' A name of an IPUMS project, one of:
#'   "IPUMS-USA", "IPUMS-CPS", "IPUMS-International", "IPUMS-DHS",
#'   "ATUS-X", "AHTUS-X", "MTUS-X", "NHIS", "Higher Ed", "NHGIS",
#'   or "IPUMS Terra"
#'@export
ipums_website <- function(x, var, project = NULL) {
  UseMethod("ipums_website")
}

#'@export
ipums_website.ddi <- function(x, var, project = NULL) {
  if (is.null(project)) project <- x$ipums_project
  url <- get_ipums_url(var, project)
  shell.exec(url)
}

#'@export
ipums_website.default <- function(x, var, project = NULL) {
  if (is.null(project)) project <- attributes(x)[["ipums_project"]]
  url <- get_ipums_url(var, project)
  shell.exec(url)
}

get_ipums_url <- function(var, project) {
  if (is.null(project)) {
    stop(paste0("Project not found. Please specify the project name using 'project' argument. ",
                "Options include: ", paste(acceptable_projects, collapse = ", ")
    ))
  }
  # Ignore case
  project <- toupper(project)

  if (project %in% c("NHGIS")) {
    out <- "https://data2.nhgis.org/main"
    message("Cannot give a variable-specific URL for NHGIS project.")
  } else if (project %in% c("IPUMS TERRA")) {
    out <- "https://data.terrapop.org/"
    message("Cannot give a variable-specific URL for Terrapop project.")
  } else {
    out <- switch(
      project,
      `IPUMS-USA` = paste0("https://usa.ipums.org/usa-action/variables/", var),
      `IPUMS-CPS` = paste0("https://cps.ipums.org/cps-action/variables/", var),
      `IPUMS-INTERNATIONAL` = paste0("https://international.ipums.org/international-action/variables/", var),
      `IPUMS-DHS` = paste0("https://www.idhsdata.org/idhs-action/variables/", var),
      `ATUS-X` = paste0("https://www.atusdata.org/atus-action/variables/", var),
      `AHTUS-X` = paste0("https://www.ahtusdata.org/ahtus-action/variables/", var),
      `MTUS-X` = paste0("https://www.mtusdata.org/atus-action/variables/", var),
      `NHIS` = paste0("https://www.mtusdata.org/atus-action/variables/", var),
      `HIGHER ED` = paste0("https://www.mtusdata.org/atus-action/variables/", var),
      NULL
    )

    if (is.null(out)) {
      stop(paste0(
        "Unexpected project '", project, "'. ",
        "Options include: ", paste(acceptable_projects, collapse = ", ")
      ))
    }
  }
  out
}

acceptable_projects <- c(
  "IPUMS-USA", "IPUMS-CPS", "IPUMS-International", "IPUMS-DHS", "ATUS-X",
  "AHTUS-X", "MTUS-X", "NHIS", "Higher Ed", "NHGIS", "IPUMS Terra"
)

# Example URLS
# USA Ex: https://usa.ipums.org/usa-action/variables/ABSENT
# CPS Ex: https://cps.ipums.org/cps-action/variables/ABSENT
# IPUMSI Ex: https://international.ipums.org/international-action/variables/ABROADCHD
# DHS Ex: https://www.idhsdata.org/idhs-action/variables/ABDOMINYR
# ATUS Ex: https://www.atusdata.org/atus-action/variables/WT06 (Time use vars won't work...)
# AHTUS Ex: https://www.ahtusdata.org/ahtus-action/variables/EPNUM (Time use vars won't work...)
# MTUS Ex: https://www.mtusdata.org/mtus-action/variables/SAMPLE
# IHIS Ex: https://ihis.ipums.org/ihis-action/variables/ABGASTRUBYR
# Higher Ed Ex: https://highered.ipums.org/highered-action/variables/ACADV

# NHGIS Ex: https://data2.nhgis.org/main (can't get to specific variable...)
# Terrapop Ex: https://data.terrapop.org/ (can't get to specific variable...)
