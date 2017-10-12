# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

# readr does not offer users the ability to override defaults, so we
# only need to worry about encoding, which unfortunately, we are not
# consistent about.
# Default to ISO-8859-1 (eg latin1), because most IPUMS data appears to
# use this. Notably, DDI's explicitly declare it often, and NHGIS is
# (some county names have diacritics).
# However, UTF-8 appears in Terrapop Area extracts (and maybe microdata?)
ipums_locale <- function(encoding = NULL) {
  if (is.null(encoding)) encoding <- "ISO-8859-1"
  readr::locale(encoding = encoding)
}

# Helper function for using dplyr's select functions to select
# rows based on values in a column of a data.frame.
select_var_rows <- function(df, vars, filter_var = "var_name") {
  if (!quo_is_null(vars)) {
    vars <- dplyr::select_vars(df[[filter_var]], !!vars)
    df <- dplyr::filter(df, .data[[!!filter_var]] %in% vars)
  }
  df
}


find_files_in_zip <- function(
  file,
  name_ext = NULL,
  name_select = quo(NULL),
  multiple_ok = FALSE
) {
  file_names <- utils::unzip(file, list = TRUE)$Name

  if (!is.null(name_ext)) file_names <- stringr::str_subset(file_names, paste0("\\.", name_ext, "$"))
  if (!quo_is_null(name_select)) file_names <- dplyr::select_vars(file_names, !!name_select)

  if (!multiple_ok && length(file_names) > 1) {
    arg_name <- deparse(substitute(name_select))
    stop(paste0(
      "Multiple files found, please use the `", arg_name, "` argument to ",
      "specify which you want to load.\n", paste(file_names, collapse = ", ")
    ), call. = FALSE)
  }

  unname(file_names)
}

#' Add IPUMS variable attributes to a data.frame
#'
#' Add variable attributes from an IPUMS DDI to the variables in a data.frame.
#' This function is usually called automatically for you inside of the read_*
#' functions (such as read_ipums_micro or read_ipums_nhgis), but they can
#' be useful other times as well. For example, if you store the data in
#' a database, you can store the data without attributes in the database
#' and add them on after loading a subset into a data.frame.
#'
#' Attribute \code{val_labels} adds the haven::labelled class attributes and
#' the corresponding value labels for variables that have value labels.
#'
#' Attribute \code{var_label} Adds a short summary of the variable's
#' contents that to the attribute "label". This label is viewable in the
#' Rstudio Viewer.
#'
#' Attribute \code{var_desc} Adds a longer summary of the variable's
#' contents to the attribute "var_desc" when available.
#'
#' @param data A data.frame
#' @param var_info An \code{ipums_ddi} object or a data.frame with the
#'   variable information (equivalent to getting ipums_var_info on a DDI).
#' @param var_attrs One or more of \code{val_labels}, \code{var_label} and
#'   \code{var_desc} describing what kinds of attributes you want to add.
#'   If NULL, will not add any attributes.
#' @return A \code{tbl_df} data.frame with data and IPUMS attributes
#' @examples
#'   ddi_file <- ripums_example("cps_00006.xml")
#'   ddi <- read_ipums_ddi(ddi_file)
#'   cps <- read_ipums_micro(ddi, var_attrs = NULL) # Don't load with attributes
#'
#'   ipums_var_desc(cps$YEAR) # Not available
#'
#'   # But, we can add on attributes after loading
#'   cps_with_attr <- set_ipums_var_attributes(cps, ddi)
#'   ipums_var_desc(cps_with_attr$YEAR)
#'
#' @export
set_ipums_var_attributes <- function(
  data,
  var_info,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  if (inherits(var_info, "ipums_ddi")) var_info <- var_info$var_info
  if (is.null(var_info) || is.null(var_attrs)) return(data)

  var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  add_val_labels <- "val_labels" %in% var_attrs
  add_var_labels <- "var_label" %in% var_attrs
  add_var_desc <- "var_desc" %in% var_attrs

  purrr::pwalk(var_info, function(var_name, ...) {
    x <- list(...)
    # Don't fail if we have a variable that doesn't match for some reason
    if (var_name %in% names(data)) {
      if (add_val_labels && !is.null(x$val_labels) && nrow(x$val_labels) > 0) {
        lbls <- purrr::set_names(x$val_labels$val, x$val_labels$lbl)
        data[[var_name]] <<- haven::labelled(data[[var_name]], lbls)
      }
      if (add_var_labels && !is.null(x$var_label)) {
        data[[var_name]] <<- rlang::set_attrs(data[[var_name]], label = x$var_label)
      }
      if (add_var_desc && !is.null(x$var_desc)) {
        data[[var_name]] <<- rlang::set_attrs(data[[var_name]], var_desc = x$var_desc)
      }
    }
  })
  data
}

set_imp_decim <- function(data, var_info) {
  if (is.null(var_info)) return(data)

  purrr::pwalk(var_info, function(var_name, ...) {
    x <- list(...)
    if (!is.null(x$imp_decim) && is.numeric(data[[var_name]])) {
      data[[var_name]] <<- data[[var_name]] / (10 ^ x$imp_decim)
    }
  })
  data
}

load_sf_namespace <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(paste0(
      "Package 'sf' must be installed to read boundary files as spacial objects.",
      " Please run command `install.packages('sf')` to continue."
    ))
  }
}

load_rgdal_namespace <- function() {
  if (!requireNamespace("rgdal", quietly = TRUE)) {
    stop(paste0(
      "Package 'rgdal' must be installed to read boundary files as spacial objects.",
      " Please run command `install.packages('rgdal')` to continue."
    ))
  }
}

file_is_zip <- function(file) {
  ipums_file_ext(file) == ".zip"
}

# Treat .gz as an incomplete file extension
ipums_file_ext <- function(file) {
  ext <- paste0(".", tools::file_ext(file))
  if (ext == ".gz") {
    ext_part1 <- tools::file_ext(tools::file_path_sans_ext(file))
    if (ext_part1 != "") ext <- paste0(".", ext_part1, ext)
  }
  ext
}

file_as_ext <- function(file, ext) {
  paste0(tools::file_path_sans_ext(file, compression = TRUE), ext)
}

# Adapted from readr:::show_progress
show_readr_progress <- function(verbose) {
  verbose && isTRUE(getOption("readr.show_progress")) && interactive() &&
    is.null(getOption("knitr.in.progress"))
}

tbl_print_for_message <- function(x, n = 5) {
  x <- dplyr::as_data_frame(x)
  out <- utils::capture.output(print(x, n = n))
  out <- paste(out[-1], collapse = "\n")
  out
}

# TODO: Could adapt readr parse_number to be much faster than this.
#       Can't use it directly because parse_number ignores when there
#       are letters and numbers, while readr::parse_guess thinks leading
#       0's means it is string.
custom_parse_number <- function(x) {
  converted <- suppressWarnings(as.numeric(x))
  if (all(is.na(converted) == is.na(x))) return(converted) else return(x)
}

