#' Add value labels to a dataset
#'
#' Takes a dataset and a format library (eg from a variables control file)
#' and adds the value labels to the dataset. Because R doesn't handle labelled
#' values in the same way as other statistical packages, there are several options
#' for the format in which the value labels are stored. See Details for more information.
#' @export
#' @param data The dataset to convert
#' @param vnames A vector of variable names
#' @param values A vector of nested data.frames with value and label columns
#'               that describes what values should be assigned which label.
#'               Currently is_missing_value column is ignored.
#'
add_value_labels <- function(data, vnames, values) {
  if (is.null(values)) values <- vector("list", length(vnames))
  var_info <- dplyr::tibble(name = vnames, values = values)

  labelled_vars <- dplyr::filter_(var_info, ~purrr::map_lgl(values, ~length(.) > 0))

  for (iii in seq_len(nrow(labelled_vars))) {
    var_name <- labelled_vars$name[iii]
    var_labels <- labelled_vars$values[[iii]]

    # Sometimes we have dups in our coding, this avoids an unwanted warning if
    # the whole row is a duplicate (still get warning if same value is labelled
    # differently on different rows)
    var_labels <- dplyr::distinct(var_labels)

    data_values <- data[[var_name]]

    # Excel (which has the data dictionary) doesn't preserve leading 0's
    # If they exist in the original data
    if (is.character(data_values)) {
      data_values <- sub(data_values, pattern = "^0+([0-9])+$", replacement = "\\1")
    }

    vlabels <- purrr::set_names(var_labels$value, var_labels$label)
    data[[var_name]] <- haven::labelled(data_values, vlabels)
  }
  data
}

#' Add variable labels to a dataset
#'
#' Takes a dataset and a format library (eg from a variables control file)
#' and adds the variable labels to the dataset. R isn't great about hanging onto
#' this kind of metainformation, so other functions may accidentally delete it.
#'
#' @export
#' @param data The dataset to convert
#' @param vnames The names of the variables to be labelled
#' @param vlabels The labels to be added
add_variable_labels <- function(data, vnames, vlabels) {
  vinfo <- dplyr::data_frame(name = vnames, label = vlabels)
  vinfo <- dplyr::filter_(vinfo, ~!is.na(label))
  for (iii in seq_len(nrow(vinfo))) {
    var_name <- vinfo$name[[iii]]
    var_label <- vinfo$label[[iii]]

    attr(data[[var_name]], "label") <- var_label
  }
  data
}
