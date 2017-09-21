# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

# Consolidate all project-specific code into one place to preserve sanity

# Project configs include:
# rectype_trans:
# Either a named vector indicating a translation of how the RECTYPE is stored in
# the data vs the DDI, or NULL to indicate no translation is necessary
# var_url:
# TRUE/FALSE to indicate whether the variables (in general) have a specific URL
# we can guess at.
# url_function:
# A function that returns a URL (can either be to the specific variable, or
# just to a general website depending on the value of var_url)

# Handle the RECTYPE converstion -----
# Hopefully can get rid of this soon! (redmine 14283)
convert_rectype <- function(rt_trans, rt_data) {
  unname(rt_trans)[match(rt_data, names(rt_trans))]
}

# Project specific configurations ------
proj_config <- list()

proj_config[["IPUMS-USA"]] <- list(
  rectype_trans = NULL,
  var_url = TRUE,
  url_function = function(var) paste0("https://usa.ipums.org/usa-action/variables/", var)
)

proj_config[["IPUMS-CPS"]] <- list(
  rectype_trans = NULL,
  var_url = TRUE,
  url_function = function(var) paste0("https://cps.ipums.org/cps-action/variables/", var)
)

proj_config[["IPUMS-INTERNATIONAL"]] <- list(
  rectype_trans = NULL,
  var_url = TRUE,
  url_function = function(var) paste0("https://international.ipums.org/international-action/variables/", var)
)

# Currently no DDI's for DHS so it is not supported
proj_config[["IPUMS-DHS"]] <- list(
  rectype_trans = NULL,
  var_url = TRUE,
  url_function = function(var)  paste0("https://www.idhsdata.org/idhs-action/variables/", var)
)

proj_config[["NHGIS"]] <- list(
  rectype_trans = NULL,
  var_url = FALSE,
  url_function = function(var)  paste0("https://data2.nhgis.org/main")
)

proj_config[["IPUMS TERRA"]] <- list(
  rectype_trans = NULL,
  var_url = FALSE,
  url_function = function(var)  paste0("https://data.terrapop.org/")
)

proj_config[["ATUS-X"]] <- list(
  rectype_trans = c(`1` = "H", `2` = "P", `3` = "A", `4` = "W", `5` = "R"),
  var_url = TRUE,
  url_function = function(var) paste0("https://atus.ipums.org/atus-action/variables/", var)
)

proj_config[["AHTUS-X"]] <- list(
  rectype_trans = c(`1` = "H", `2` = "P", `3` = "A", `4` = "W", `5` = "R"),
  var_url = TRUE,
  url_function = function(var) paste0("https://ahtus.ipums.org/ahtus-action/variables/", var)
)

proj_config[["MTUS-X"]] <- list(
  rectype_trans = c(`1` = "H", `2` = "P", `3` = "A", `4` = "W", `5` = "R"),
  var_url = TRUE,
  url_function = function(var) paste0("https://mtus.ipums.org/mtus-action/variables/", var)
)

proj_config[["NHIS"]] <- list(
  rectype_trans = c(`1` = "H", `2` = "P", `3` = "I"),
  var_url = TRUE,
  url_function = function(var) paste0("https://ihis.ipums.org/ihis-action/variables/", var)
)

proj_config[["HIGHER ED"]] <- list(
  rectype_trans = NULL,
  var_url = TRUE,
  url_function = function(var) paste0("https://highered.ipums.org/highered-action/variables/", var)
)

default_config <- list(
  rectype_trans = NULL,
  var_url = FALSE,
  url_function = function(var) "https://www.ipums.org"
)

get_proj_config <- function(proj) {
  out <- proj_config[[toupper(proj)]] # Ignore case
  if (is.null(out)) out <- default_config
  out
}

all_proj_names <- function() {
  names(proj_config)
}

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
