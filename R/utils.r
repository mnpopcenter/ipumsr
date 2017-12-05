# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

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


find_files_in <- function(
  file,
  name_ext = NULL,
  name_select = quo(NULL),
  multiple_ok = FALSE
) {
  if (file_is_zip(file)) {
    file_names <- utils::unzip(file, list = TRUE)$Name
  } else if (file_is_dir(file)) {
    file_names <- dir(file)
  } else {
    stop(paste0(
      "Expected a folder or a zip file to look for files in, but got:\n",
      file
    ))
  }


  if (!is.null(name_ext)) file_names <- stringr::str_subset(file_names, paste0("\\.", name_ext, "$"))
  if (!quo_is_null(name_select)) file_names <- dplyr::select_vars(file_names, !!name_select)

  if (!multiple_ok && length(file_names) > 1) {
    arg_name <- deparse(substitute(name_select))
    stop(paste(
      custom_format_text(
        "Multiple files found, please use the `", arg_name, "` argument to ",
        "specify which you want to load.", indent = 2, exdent = 2
      ),
      custom_format_text(
        paste(file_names, collapse = ", "), indent = 4, exdent = 4
      ),
      sep = "\n"
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
#'   ddi_file <- ipums_example("cps_00006.xml")
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

  if (!"val_labels" %in% var_attrs || is.null(var_info$val_labels)) {
    var_info$val_labels <- vector(mode = "list", length = nrow(var_info))
  }
  if (!"var_label" %in% var_attrs || is.null(var_info$var_label)) {
    var_info$var_label <- NA_character_
  }
  if (!"var_desc" %in% var_attrs || is.null(var_info$var_desc)) {
    var_info$var_desc <- NA_character_
  }

  # Don't fail if we have a variable that doesn't match for some reason
  var_info <- dplyr::filter(var_info, .data$var_name %in% names(data))

  # Give error message if type doesn't match between value labels and variable
  # as haven::labelled would

  class_data <- dplyr::data_frame(
    var_name = names(data),
    d_type = purrr::map(data, typeof),
    d_is_num = purrr::map_lgl(data, is.numeric)
  )
  class_labels <- dplyr::data_frame(
    var_name = var_info$var_name,
    l_type = purrr::map(var_info$val_labels, ~typeof(.$val)),
    l_is_num = purrr::map_lgl(
      var_info$val_labels,
      ~ifelse(nrow(.) == 0, NA, is.numeric(.$val))
    )
  )
  class_labels <- dplyr::filter(class_labels, !is.na(.data$l_is_num))
  class_join <- dplyr::inner_join(class_data, class_labels, by = "var_name")
  class_join <- dplyr::mutate(
    class_join,
    coercible =  purrr::map2_lgl(.data$d_type, .data$l_type, ~.x == .y) |
      (.data$d_is_num & .data$l_is_num)
  )
  class_join <- dplyr::filter(class_join, !.data$coercible)
  if (nrow(class_join) > 0) {
    stop(paste0(
      custom_format_text("Data and labels are not of the same type for variables: "),
      "\n",
      custom_format_text(paste(class_join$var_name, collapse = ", "), indent = 4, exdent = 4)
    ))
  }

  # Convert data frame of value labels to named vector as the labelled class expects
  var_info$val_labels <- purrr::map(var_info$val_labels, function(x) {
    if (length(x) == 0 || nrow(x) == 0) NULL else purrr::set_names(x$val, x$lbl)
  })

  set_ipums_var_attributes_(data, var_info)
}

set_imp_decim <- function(data, var_info) {
  if (is.null(var_info)) return(data)

  var_info <- dplyr::filter(var_info, .data$var_name %in% names(data))

  # Set imp_decim to 0 when missing or variable is character
  if (is.null(var_info$imp_decim)) var_info$imp_decim <- 0
  var_info$imp_decim[is.na(var_info$imp_decim)] <- 0

  non_numeric_vars <- purrr::map_lgl(data, ~!is.numeric(.))
  non_numeric_vars <- names(data)[non_numeric_vars]
  var_info$imp_decim[var_info$var_name %in% non_numeric_vars] <- 0

  set_imp_decim_(data, var_info)
}

load_sf_namespace <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(custom_format_text(
      "Package 'sf' must be installed to read boundary files as spacial objects.",
      " Please run command `install.packages('sf')` to continue.",
      indent = 2, exdent = 2
    ))
  }
}

load_rgdal_namespace <- function() {
  if (!requireNamespace("rgdal", quietly = TRUE)) {
    stop(custom_format_text(
      "Package 'rgdal' must be installed to read boundary files as spacial objects.",
      " Please run command `install.packages('rgdal')` to continue.",
      indent = 2, exdent = 2
    ))
  }
}

file_is_zip <- function(file) {
  ipums_file_ext(file) == ".zip"
}

file_is_dir <- function(file) {
  ipums_file_ext(file) == ""
}

# Treat .gz as an incomplete file extension
ipums_file_ext <- function(file) {
  ext <- tools::file_ext(file)
  if (ext != "") ext <- paste0(".", ext)
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
custom_parse_double <- function(x) {
  converted <- suppressWarnings(as.double(x))
  if (all(is.na(converted) == is.na(x))) return(converted) else return(x)
}

custom_parse_integer <- function(x) {
  converted <- suppressWarnings(readr::parse_integer(x))
  if (all(is.na(converted) == is.na(x))) return(converted) else return(x)
}


custom_format_text <- function(..., indent = 0, exdent = 0) {
  text <- paste0(...)
  text <- stringr::str_split(text, "\n")
  text <- stringr::str_wrap(text[[1]], indent = indent, exdent = exdent)
  text <- paste(text, collapse = "\n")
  text
}

custom_cat <- function(..., indent = 0, exdent = 0) {
  cat(custom_format_text(..., indent = indent, exdent = exdent))
}

custom_check_file_exists <- function(file, extra_ext = NULL) {
  if (length(file) == 0) stop("Expected filepath but got empty.")
  if (length(file) != 1 && !is.null(extra_ext)) stop("Bad filename argument.")

  # If a path is passed with a trailing "/", file.exists returns FALSE, so
  # this is a way to remove that trailing /
  file <- file.path(dirname(file), basename(file))

  if (!is.null(extra_ext)) {
    file <- purrr::map_chr(extra_ext, ~file_as_ext(file, .))
  }

  exists_check <- file.exists(file)

  if (!any(exists_check)) {
    file <- file[1]
    if (dirname(file) == ".") {
      stop(paste0(
        "Could not find file named '", file, "' in current working directory:\n  ",
        getwd(), "\nDo you need to change the directory with `setwd()`?"
      ))
    } else {
      stop(paste0(
        "Could not find file, check the path in argument 'file':\n  ",
        file
      ))
    }
  } else {
    file[exists_check][1]
  }
}


path_is_zip_or_dir <- function(file) {
  ext <- tools::file_ext(file)
  ext == "zip" || ext == ""
}
