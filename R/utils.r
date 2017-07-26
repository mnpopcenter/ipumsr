# Helper function for using dplyr's select functions to select
# rows based on values in a column of a data.frame.
select_var_rows <- function(df, vars, filter_var = "var_name") {
  if (!quo_is_null(vars)) {
    vars <- dplyr::select_vars(df[[filter_var]], !!vars)
    df <- dplyr::filter(df, .data[[!!filter_var]] %in% vars)
  }
  df
}


find_files_in_zip <- function(file, name_ext = NULL, name_regex = NULL, multiple_ok = FALSE) {
  file_names <- utils::unzip(file, list = TRUE)$Name

  if (!is.null(name_ext)) file_names <- stringr::str_subset(file_names, paste0("\\.", name_ext, "$"))

  if (!is.null(name_regex)) file_names <- stringr::str_subset(file_names, name_regex)

  if (!multiple_ok && length(file_names) > 1) {
    arg_name <- deparse(substitute(name_regex))
    stop(paste0(
      "Multiple files found, please use the `", arg_name, "` argument to ",
      " specify which you want to load.\n", paste(file_names, collapse = ", ")
    ), call. = FALSE)
  }

  file_names
}
