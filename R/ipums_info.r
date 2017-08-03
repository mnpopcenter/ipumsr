# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport


#' Get IPUMS variable information
#'
#' Get IPUMS metadata information about variables loaded into R. Will try to read
#' the metadata fron the loaded datasets, but it is more reliable to load the DDI
#' into a separate object and use it instead.
#'
#' @param object A DDI object (loaded with \code{\link{read_ddi}}), a data.frame
#'   with ipums metadata attached, or a single column from an ipums data.frame.
#' @param vars dplyr \code{\link[dplyr]{select}}-stlye notation for the variables to
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
#'\code{ipums_value_labels()} loads the value labels for a single variable.
#'
#' Note that many R functions drop attributes that provide this information.
#' In order to make sure that they are available, it is best to keep a copy of the
#' separate from the data your are manipulating using \code{\link{read_ddi}}. Then
#' you can refer to the IPUMS documentation in this object.
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
    var_label = if (is.null(obj_info$label)) NA_character_ else obj_info$label,
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
#' Gets information about citation and conditions from objects with
#' IPUMS metadata, like a DDI or a loaded extract. Because of how some
#' R functions drop attributes, it is best to load a DDI object separately
#' from the data and get the conditions and citation from this object.
#'
#' @param object A DDI object (loaded with \code{\link{read_ddi}}), or a data.frame
#'   with ipums metadata attached.
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

#' @export
ipums_conditions.default <- function(object) {
  atts <- attributes(object)
  out <- atts$conditions
  if (!is.null(atts$citation)) out <- paste0("\n\n", atts$citation, "\n\n")
  out
}
