# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr


#' Read data from an IPUMS extract
#'
#' Reads a dataset downloaded from the IPUMS extract system.
#' For IPUMS projects with microdata, it relies on a downloaded
#' DDI codebook and a fixed-width file. Loads the data with
#' value labels (using \code{\link[haven]{labelled}} format)
#' and variable labels. See 'Details' for more information on
#' how record types are handled by the ipumsr package.
#'
#' Some IPUMS projects have data for multiple types of records
#' (eg Household and Person). When downloading data from many of these
#' projects you have the option for the IPUMS extract system
#' to "rectangularize" the data, meaning that the data is
#' transformed so that each row of data represents only one
#' type of record.
#'
#' There also is the option to download "hierarchical" extracts,
#' which are a single file with record types mixed in the rows.
#' The ipumsr package offers two methods for importing this data.
#'
#' \code{read_ipums_micro} loads this data into a "long" format
#' where the record types are mixed in the rows, but the variables
#' are \code{NA} for the record types that they do not apply to.
#'
#' \code{read_ipums_micro_list} loads the data into a list of
#' data frames objects, where each data frame contains only
#' one record type. The names of the data frames in the list
#' are the text from the record type labels without 'Record'
#' (often 'HOUSEHOLD' for Household and 'PERSON' for Person).
#'
#' @param ddi Either a filepath to a DDI xml file downloaded from
#'   the website, or a \code{ipums_ddi} object parsed by \code{\link{read_ipums_ddi}}
#' @param vars Names of variables to load. Accepts a character vector of names, or
#'  \code{\link{dplyr_select_style}} conventions. For hierarchical data, the
#'  rectype id variable will be added even if it is not specified.
#' @param n_max The maximum number of records to load.
#' @param data_file Specify a directory to look for the data file.
#'   If left empty, it will look in the same directory as the DDI file.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @param rectype_convert (Usually determined by project) A named vector
#'   indicating a conversion from the rectype in data to DDI. Not usually
#'   needed to be specified by the user.
#' @param var_attrs Variable attributes to add from the DDI, defaults to
#'   adding all (val_labels, var_label and var_desc). See
#'   \code{\link{set_ipums_var_attributes}} for more details.
#' @return \code{read_ipums_micro} returns a single tbl_df data frame, and
#'   \code{read_ipums_micro_list} returns a list of data frames, named by
#'   the Record Type. See 'Details' for more
#'   information.
#' @examples
#'   # Rectangular example file
#'   cps_rect_ddi_file <- ipums_example("cps_00006.xml")
#'
#'   cps <- read_ipums_micro(cps_rect_ddi_file)
#'   # Or load DDI separately to keep the metadata
#'   ddi <- read_ipums_ddi(cps_rect_ddi_file)
#'   cps <- read_ipums_micro(ddi)
#'
#'   # Hierarchical example file
#'   cps_hier_ddi_file <- ipums_example("cps_00010.xml")
#'
#'   # Read in "long" format and you get 1 data frame
#'   cps_long <- read_ipums_micro(cps_hier_ddi_file)
#'   head(cps_long)
#'
#'   # Read in "list" format and you get a list of multiple data frames
#'   cps_list <- read_ipums_micro_list(cps_hier_ddi_file)
#'   head(cps_list$PERSON)
#'   head(cps_list$HOUSEHOLD)
#'
#' @family ipums_read
#' @export
read_ipums_micro <- function(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  rectype_convert = NULL,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) custom_cat(ipums_conditions(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  if (ddi$file_type == "hierarchical") {
    out <- read_ipums_hier(ddi, vars, n_max, "long", data_file, verbose, rectype_convert, var_attrs)
  } else if (ddi$file_type == "rectangular") {
    out <- read_ipums_rect(ddi, vars, n_max, data_file, verbose, var_attrs)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

  out
}

#' @export
#' @rdname read_ipums_micro
read_ipums_micro_list <- function(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  rectype_convert = NULL,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) custom_cat(ipums_conditions(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  if (ddi$file_type == "hierarchical") {
    out <- read_ipums_hier(ddi, vars, n_max, "list", data_file, verbose, rectype_convert, var_attrs)
  } else if (ddi$file_type == "rectangular") {
    out <- read_ipums_rect(ddi, vars, n_max, data_file, verbose, var_attrs)
    warning("Assuming data rectangularized to 'P' record type")
    out <- list(P = out)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

  out
}


read_ipums_hier <- function(
  ddi, vars, n_max, data_structure, data_file, verbose, rectype_convert, var_attrs
) {
  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    stop("Hierarchical data cannot be read as csv.")
  }
  all_vars <- ddi$var_info

  rec_vinfo <- dplyr::filter(all_vars, .data$var_name == ddi$rectype_idvar)
  if (nrow(rec_vinfo) > 1) stop("Cannot support multiple rectype id variables.", call. = FALSE)

  all_vars <- select_var_rows(all_vars, vars)
  if (!rec_vinfo$var_name %in% all_vars$var_name) {
    if (verbose && data_structure == "long") {
      cat(paste0("Adding rectype id var '", rec_vinfo$var_name, "' to data.\n"))
    }
    all_vars <- dplyr::bind_rows(rec_vinfo, all_vars)
  }

  nonrec_vinfo <- dplyr::filter(all_vars, .data$var_name != ddi$rectype_idvar)
  nonrec_vinfo <- tidyr::unnest_(nonrec_vinfo, "rectypes", .drop = FALSE)

  if (verbose) cat("Reading data...\n")
  if (n_max == Inf) n_max <- -1
  raw <- read_check_for_negative_bug(
    readr::read_lines_raw,
    data_file,
    progress = show_readr_progress(verbose),
    n_max = n_max
  )

  if (verbose) cat("Parsing data...\n")
  if (data_structure == "long") {
    out <- read_raw_to_df_long(raw, rec_vinfo, all_vars, ddi$file_encoding)

    out <- set_ipums_var_attributes(out, all_vars, var_attrs)
    out <- set_imp_decim(out, all_vars)
  } else if (data_structure == "list") {
    out <- read_raw_to_df_list(raw, rec_vinfo, all_vars, ddi$file_encoding)
    for (rt in names(out)) {
      rt_vinfo <- all_vars[purrr::map_lgl(all_vars$rectypes, ~rt %in% .), ]
      out[[rt]] <- set_ipums_var_attributes(out[[rt]], rt_vinfo, var_attrs)
      out[[rt]] <- set_imp_decim(out[[rt]], rt_vinfo)
    }
    # If value labels for rectype are available use them to name data.frames
    rt_lbls <- rec_vinfo$val_labels[[1]]
    matched_lbls <- match(names(out), rt_lbls$val)
    if (all(!is.na(matched_lbls))) {
      # Can use the value labels
      rt_lbls <- rt_lbls$lbl[matched_lbls]
      # Clean it up a bit though: all upper case
      rt_lbls <- toupper(rt_lbls)
      # drop trailing 'record'
      rt_lbls <- stringr::str_replace_all(rt_lbls, " RECORD$", "")
      # and replace blank space with _
      rt_lbls <- stringr::str_replace_all(rt_lbls, "[:blank:]", "_")
      names(out) <- rt_lbls
    }


  }
  out
}

read_ipums_rect <- function(ddi, vars, n_max, data_file, verbose, var_attrs) {
  all_vars <- select_var_rows(ddi$var_info, vars)

  col_types <- purrr::map(all_vars$var_type, function(x) {
    if (x == "numeric") out <- readr::col_double()
    else if(x == "character") out <- readr::col_character()
    else if (x == "integer") out <- readr::col_integer()
    out
  })
  names(col_types) <- all_vars$var_name
  col_types <- do.call(readr::cols, col_types)

  col_positions <- readr::fwf_positions(
    start = all_vars$start,
    end = all_vars$end,
    col_names = all_vars$var_name
  )

  is_fwf <- ipums_file_ext(data_file) %in% c(".dat", ".dat.gz")
  is_csv <- ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")

  if (is_fwf) {
    out <- read_check_for_negative_bug(
      readr::read_fwf,
      data_file,
      col_positions,
      col_types,
      n_max = n_max,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
  } else if (is_csv) {
    out <- read_check_for_negative_bug(
      readr::read_csv,
      data_file,
      col_types = col_types,
      n_max = n_max,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
  } else {
    stop("Unrecognized file type.")
  }
  out <- set_ipums_var_attributes(out, all_vars, var_attrs)
  out <- set_imp_decim(out, all_vars)

  out
}

# Check for https://github.com/tidyverse/readr/issues/663
read_check_for_negative_bug <- function(readr_f, data_file, ...) {
  lines <- purrr::safely(readr_f)(data_file, ...)
  if (!is.null(lines$error)) {
    error_message <- as.character(lines$error)
    if (tools::file_ext(data_file) %in% c("gz", "zip") &&
        stringr::str_detect(error_message, "negative length")) {
      stop(call. = FALSE, paste0(
        "Could not read data file, possibly because of a bug in readr when loading ",
        "large zip files. Try unzipping the .gz file and reading the data again."
      ))
    } else {
      stop(error_message, call. = FALSE)
    }
  } else {
    lines <- lines$result
  }
  lines
}


read_raw_to_df_long <- function(raw, rec_vinfo, all_vars, encoding) {
  # Simplify structures before sending to C++
  rt_info <- prep_rt_info_cpp(rec_vinfo)
  var_names <- all_vars$var_name
  vars_by_rt <- prep_all_var_info_cpp(all_vars)
  encoding <- prep_encoding_cpp(encoding)

  # Read in data, add names and convert to numeric
  out <- raw_to_df_hier_long(raw, length(var_names), rt_info, vars_by_rt, encoding)
  names(out) <- var_names
  out <- tibble::as_tibble(out)

  numeric_vars <- all_vars$var_name[all_vars$var_type == "numeric"]
  out <- dplyr::mutate_at(out, numeric_vars, readr::parse_number)
  integer_vars <- all_vars$var_name[all_vars$var_type == "integer"]
  out <- dplyr::mutate_at(out, integer_vars, readr::parse_integer)
  out
}

read_raw_to_df_list <- function(raw, rec_vinfo, all_vars, encoding) {
  # Simplify structures before sending to C++
  rt_info <- prep_rt_info_cpp(rec_vinfo)
  var_names <- all_vars$var_name
  vars_by_rt <- prep_all_var_info_cpp(all_vars)
  encoding <- prep_encoding_cpp(encoding)

  # Read in data, add names and convert to numeric
  out <- raw_to_df_hier_list(raw, rt_info, vars_by_rt, encoding)
  names(out) <- names(vars_by_rt)

  for (rt in names(vars_by_rt)) {
    names(out[[rt]]) <- vars_by_rt[[rt]]$var_name
    out[[rt]] <- tibble::as_tibble(out[[rt]])

    numeric_vars <- vars_by_rt[[rt]]$var_name[vars_by_rt[[rt]]$var_type == "numeric"]
    out[[rt]] <- dplyr::mutate_at(out[[rt]], numeric_vars, readr::parse_number)

    integer_vars <- vars_by_rt[[rt]]$var_name[vars_by_rt[[rt]]$var_type == "integer"]
    out[[rt]] <- dplyr::mutate_at(out[[rt]], integer_vars, readr::parse_integer)
  }

  out
}

prep_rt_info_cpp <- function(rec_vinfo) {
  if (nrow(rec_vinfo) > 1) stop("Only one rectype variable allowed.")

  list(
    start = as.integer(rec_vinfo$start - 1),
    width = as.integer(rec_vinfo$end - rec_vinfo$start + 1)
  )
}

prep_all_var_info_cpp <- function(all_vars) {
  out <- dplyr::mutate(
    all_vars,
    start = as.integer(.data$start - 1),
    end = as.integer(.data$end),
    width = as.integer(.data$end - .data$start),
    var_pos = as.integer(seq_along(.data$width) - 1)
  )
  out <- dplyr::select(
    out,
    dplyr::one_of(c("var_name", "start", "width", "end", "var_pos", "var_type", "rectypes"))
  )
  out <- tidyr::unnest(out)
  out <- dplyr::group_by(out, .data$rectypes)
  out <- dplyr::mutate(out, max_end = max(.data$end))
  out <- tidyr::nest(out, -.data$rectypes)
  out <- tibble::deframe(out)
  out
}

prep_encoding_cpp <- function(encoding) {
  if (is.null(encoding) || encoding == "ISO-8859-1") {
    "latin1"
  } else {
    encoding
  }
}
