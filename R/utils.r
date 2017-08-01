# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport


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
    arg_name <- deparse(substitute(name_regex))
    stop(paste0(
      "Multiple files found, please use the `", arg_name, "` argument to ",
      " specify which you want to load.\n", paste(file_names, collapse = ", ")
    ), call. = FALSE)
  }

  file_names
}

set_ipums_var_attributes <- function(data, var_info, set_imp_decim = TRUE) {
  # from csv decims are explicit but DDI might say otherwise, so
  # wipe out that column if it exists
  if (is.null(var_info)) return(data)
  if (!set_imp_decim) var_info$imp_decim <- NULL

  purrr::pwalk(var_info, function(var_name, ...) {
    x <- list(...)
    # Don't fail if we have a variable that doesn't match for some reason
    if (var_name %in% names(data)) {
      if (!is.null(x$val_label) && nrow(x$val_label) > 0) {
        lbls <- purrr::set_names(x$val_label$val, x$val_label$lbl)
        data[[var_name]] <<- haven::labelled(data[[var_name]], lbls)
      }
      if (!is.null(x$var_label)) {
        data[[var_name]] <<- rlang::set_attrs(data[[var_name]], label = x$var_label)
      }
      if (!is.null(x$var_desc)) {
        data[[var_name]] <<- rlang::set_attrs(data[[var_name]], var_desc = x$var_desc)
      }
      if (!is.null(x$imp_decim) && is.numeric(data[[var_name]])) {
        data[[var_name]] <<- data[[var_name]] / (10 ^ x$imp_decim)
      }
    }
  })
  data
}

set_ipums_df_attributes <- function(data, extract_info) {
  if (!is.null(extract_info$conditions)) {
    data <- rlang::set_attrs(data, conditions = extract_info$conditions)
  }

  if (!is.null(extract_info$citation)) {
    data <- rlang::set_attrs(data, conditions = extract_info$citation)
  }
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

file_is_zip <- function(file) {
  stringr::str_sub(file, -4) == ".zip"
}
