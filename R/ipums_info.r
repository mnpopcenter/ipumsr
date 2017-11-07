# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums


#' Get IPUMS variable information
#'
#' Get IPUMS metadata information about variables loaded into R. Will try to read
#' the metadata from the loaded datasets, but it is more reliable to load the DDI
#' into a separate object and use it instead.
#'
#' @param object A DDI object (loaded with \code{\link{read_ipums_ddi}}), a data.frame
#'   with ipums metadata attached, or a single column from an ipums data.frame.
#' @param vars dplyr \code{\link[dplyr]{select}}-style notation for the variables to
#'   give information about
#' @param var select-style notation for a single variable
#'
#' \code{ipums_var_info()} loads all available variable information for one or more
#' variables into a data.frame. If \code{object} is a vector, it will include
#' the variable label, variable description and value labels. If \code{object} is
#' a data.frame, it will include it for all variables (or only those specified
#' by vars). If it is a DDI, it will also include information used to read the
#' data from disk, including start/end position in the fixed-width file, implied
#' decimals and variable type.
#'
#'\code{ipums_var_desc()} loads the variable description for a single variable.
#'
#'\code{ipums_var_label()} loads the short variable label for a single variable.
#'
#'\code{ipums_val_labels()} loads the value labels for a single variable.
#'
#' Note that many R functions drop attributes that provide this information.
#' In order to make sure that they are available, it is best to keep a copy of the
#' separate from the data your are manipulating using \code{\link{read_ipums_ddi}}. Then
#' you can refer to the IPUMS documentation in this object.
#'
#' @return
#'   \code{ipums_var_info} returns a \code{tbl_df} data frame with variable information, and
#'   the other functions return a length 1 character vector.
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))
#'
#' ipums_var_info(ddi)
#' ipums_var_desc(ddi, MONTH)
#' ipums_var_label(ddi, MONTH)
#' ipums_val_labels(ddi, MONTH)
#'
#' @export
ipums_var_info <- function(object, vars = NULL) {
  UseMethod("ipums_var_info")
}

#' @export
ipums_var_info.default <- function(object, vars = NULL) {
  obj_info <- attributes(object)

  if (length(obj_info$labels) > 0) {
  value_labels <- list(dplyr::data_frame(
    val = unname(obj_info$labels),
    lbl = names(obj_info$labels)
  ))
  } else {
    value_labels <- list(dplyr::data_frame(
      val = numeric(0),
      lbl = character(0)
    ))
  }

  dplyr::data_frame(
    var_label = if (is.null(obj_info[["label"]])) NA_character_ else obj_info[["label"]],
    var_desc = if (is.null(obj_info$var_desc)) NA_character_ else obj_info$var_desc,
    val_labels = value_labels
  )
}

#' @export
ipums_var_info.ipums_ddi <- function(object, vars = NULL) {
  vars <- enquo(vars)
  out <- object$var_info
  out <- select_var_rows(out, vars)
  out
}

#' @export
ipums_var_info.data.frame <- function(object, vars = NULL) {
  vars <- enquo(vars)
  out <- purrr::map(object, ~ipums_var_info.default(.))
  names(out) <- names(object)
  out <- dplyr::bind_rows(out, .id = "var_name")
  out <- select_var_rows(out, vars)
  out
}

#' @export
ipums_var_info.list <- function(object, vars = NULL) {
  # For hierarchical list datasets
  vars <- enquo(vars)
  out <- purrr::map_df(object, ~ipums_var_info(.))
  out <- dplyr::distinct(out, .data$var_name, .keep_all = TRUE)
  out <- select_var_rows(out, vars)
  out
}


#' @export
#' @rdname ipums_var_info
ipums_var_desc <- function(object, var = NULL) {
  UseMethod("ipums_var_desc")
}

#' @export
ipums_var_desc.default <- function(object, var = NULL) {
  out <- ipums_var_info(object, !!enquo(var))

  if (nrow(out) > 1) warning("Found multiple variables. Giving variable description from first.")
  out$var_desc[1]
}

#' @export
#' @rdname ipums_var_info
ipums_var_label <- function(object, var = NULL) {
  UseMethod("ipums_var_label")
}

#' @export
ipums_var_label.default <- function(object, var = NULL) {
  out <- ipums_var_info(object, !!enquo(var))

  if (nrow(out) > 1) warning("Found multiple variables. Giving variable label from first.")
  out$var_label[1]
}

#' @export
#' @rdname ipums_var_info
ipums_val_labels <- function(object, var = NULL) {
  UseMethod("ipums_val_labels")
}

#' @export
ipums_val_labels.default <- function(object, var = NULL) {
  out <- ipums_var_info(object, !!enquo(var))

  if (nrow(out) > 1) warning("Found multiple variables. Giving value labels from first.")
  out$val_labels[[1]]
}


#' Get IPUMS citation and conditions
#'
#' Gets information about citation and conditions from a DDI.
#'
#' @param object A DDI object (loaded with \code{\link{read_ipums_ddi}})
#'
#' @export
ipums_conditions <- function(object) {
  UseMethod("ipums_conditions")
}

#' @export
ipums_conditions.ipums_ddi <- function(object) {
  out <- paste0(object$conditions, "\n\n")
  if (!is.null(object$citation)) out <- paste0(out, object$citation, "\n\n")
  out
}


#' Get IPUMS file information
#'
#' Get IPUMS metadata information about the data file loaded into R
#' from an ipums_ddi
#'
#' @param object An ipums_ddi object (loaded with \code{\link{read_ipums_ddi}}).
#' @param type NULL to load all types, or one of "ipums_project", "extract_data",
#'   "extract_notes", "conditions" or "citation".
#' @return If \code{type} is NULL, a list with the \code{ipums_project},
#'   \code{extract_date}, \code{extract_notes}, \code{conditions}, and \code{citation}.
#'   Otherwise a string with the type of information requested in \code{type}.
#' @examples
#' ddi <- read_ipums_ddi(ipums_example("cps_00006.xml"))
#' ipums_file_info(ddi)
#' @export
ipums_file_info <- function(object, type = NULL) {
  UseMethod("ipums_file_info")
}

#' @export
ipums_file_info.default <- function(object, type = NULL) {
  return(NULL)
}

#' @export
ipums_file_info.ipums_ddi <- function(object, type = NULL) {
  if (!is.null(type)) {
    out <- object[[type]]
  } else {
    out <- object[c("ipums_project", "extract_date", "extract_notes", "conditions", "citation")]
  }
  out
}

