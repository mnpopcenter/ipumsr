#' Display IPUMS variable information
#'
#' Get IPUMS metadata information about variables loaded into R. Will try to read
#' the metadata fron the loaded datasets, but it is more reliable to load the DDI
#' into a separate object and use it instead.
#'
#' \code{ip_var_info()} loads all available variable information for one or more
#' variables into a data.frame. If \code{object} is a vector, it will include
#' the variable label, long variable label and value labels. If \code{object} is
#' a data.frame, it will include it for all variables (or only those specified
#' by vars). If it is a DDI, it will also include information used to read the
#' data from disk, including start/end position in the fixed-width file, implied
#' decimals and variable type.
#'
#'\code{ip_var_label_long()} loads the variable description for a single variable.
#'
#'\code{ip_var_label()} loads the short variable label for a single variable.
#'
#'\code{ip_value_labels()} loads the value labels for a single variable.
#'
#' Note that many R functions drop attributes that provide this information.
#' In order to make sure that they are available, it is best to keep a copy of the
#' separate from the data your are manipulating using \code{\link{read_ddi}}. Then
#' you can refer to the IPUMS documentation in this object.
#'
#' @export
ip_var_info <- function(object, vars = NULL) {
  UseMethod("ip_var_info")
}

#' @export
ip_var_info.default <- function(object, vars = NULL) {
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
    label = if (is.null(obj_info$label)) NA_character_ else obj_info$label,
    label_long = if (is.null(obj_info$label_long)) NA_character_ else obj_info$label_long,
    val_labels = value_labels
  )
}

#' @export
ip_var_info.ipums_ddi <- function(object, vars = NULL) {
  vars <- enquo(vars)
  out <- object$var_info
  if (!quo_is_null(vars)) {
    vars <- dplyr::select_vars(out$var_name, !!!vars)
    out <- dplyr::filter(out, .data$var_name %in% vars)
  }
  out
}

#' @export
ip_var_info.data.frame <- function(object, vars = NULL) {
  vars <- enquo(vars)
  out <- purrr::map(object, ~ip_var_info.default(.))
  names(out) <- names(object)
  out <- dplyr::bind_rows(out, .id = "var_name")

  if (!quo_is_null(vars)) {
    vars <- dplyr::select_vars(out$var_name, !!!vars)
    out <- dplyr::filter(out, .data$var_name %in% vars)
  }
  out
}


#' @export
#' @rdname ip_var_info
ip_var_label_long <- function(object, var = NULL) {
  UseMethod("ip_var_label_long")
}

#' @export
ip_var_label_long.default <- function(object, var = NULL) {
  out <- ip_var_info(object, !!enquo(var))

  if (nrow(out) > 1) warning("Found multiple variables. Giving long variable label from first.")
  out$label_long[1]
}

#' @export
#' @rdname ip_var_info
ip_var_label <- function(object, var = NULL) {
  UseMethod("ip_var_label")
}

#' @export
ip_var_label.default <- function(object, var = NULL) {
  out <- ip_var_info(object, !!enquo(var))

  if (nrow(out) > 1) warning("Found multiple variables. Giving variable label from first.")
  out$label[1]
}

#' @export
#' @rdname ip_var_info
ip_val_labels <- function(object, var = NULL) {
  UseMethod("ip_val_labels")
}

#' @export
ip_val_labels.default <- function(object, var = NULL) {
  out <- ip_var_info(object, !!enquo(var))

  if (nrow(out) > 1) warning("Found multiple variables. Giving value labels from first.")
  out$val_labels[[1]]
}

