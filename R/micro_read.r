# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums


#' Read data from an IPUMS extract
#'
#' Reads a dataset downloaded from the IPUMS extract system.
#' For IPUMS projects with microdata, it relies on a downloaded
#' DDI codebook and a fixed-width file. Loads the data with
#' value labels (using \code{\link[haven]{labelled}} format)
#' and variable labels. See 'Details' for more information on
#' how record types are handled by the ripums package.
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
#' The ripums package offers two methods for importing this data.
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
#' @return \code{read_ipums_micro} returns a single tbl_df data frame, and
#'   \code{read_ipums_micro_list} returns a list of data frames, named by
#'   the Record Type. See 'Details' for more
#'   information.
#' @examples
#'   # Rectangular example file
#'   cps_rect_ddi_file <- ripums_example("cps_00006.xml")
#'
#'   cps <- read_ipums_micro(cps_rect_ddi_file)
#'   # Or load DDI separately to keep the metadata
#'   ddi <- read_ipums_ddi(cps_rect_ddi_file)
#'   cps <- read_ipums_micro(ddi)
#'
#'   # Hierarchical example file
#'   cps_hier_ddi_file <- ripums_example("cps_00010.xml")
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
  n_max = -1,
  data_file = NULL,
  verbose = TRUE,
  rectype_convert = NULL
) {
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)
  # Look for zipped versions of the file or csv versions of the file if it doesn't exist
  if (!file.exists(data_file)) {
    file_dat_gz <- file_as_ext(data_file, ".dat.gz")
    file_csv <- file_as_ext(data_file, ".csv")
    file_csv_gz <- file_as_ext(data_file, ".csv.gz")

    if (file.exists(file_dat_gz)) {
      data_file <- file_dat_gz
    } else if (file.exists(file_csv)) {
      data_file <- file_csv
    } else if (file.exists(file_csv_gz)) {
      data_file <- file_csv_gz
    }
  }
  if (verbose) cat(ipums_conditions(ddi))

  vars <- enquo(vars)

  if (ddi$file_type == "hierarchical") {
    out <- read_ipums_hier(ddi, vars, n_max, "long", data_file, verbose, rectype_convert)
  } else if (ddi$file_type == "rectangular") {
    out <- read_ipums_rect(ddi, vars, n_max, data_file, verbose)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

  out <- set_ipums_df_attributes(out, ddi)
  out
}

#' @export
#' @rdname read_ipums_micro
read_ipums_micro_list <- function(
  ddi,
  vars = NULL,
  n_max = -1,
  data_file = NULL,
  verbose = TRUE,
  rectype_convert = NULL
) {
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)
  # Look for zipped versions of the file or csv versions of the file if it doesn't exist
  if (!file.exists(data_file)) {
    file_dat_gz <- file_as_ext(data_file, ".dat.gz")
    file_csv <- file_as_ext(data_file, ".csv")
    file_csv_gz <- file_as_ext(data_file, ".csv.gz")

    if (file.exists(file_dat_gz)) {
      data_file <- file_dat_gz
    } else if (file.exists(file_csv)) {
      data_file <- file_csv
    } else if (file.exists(file_csv_gz)) {
      data_file <- file_csv_gz
    }
  }
  if (verbose) cat(ipums_conditions(ddi))

  vars <- enquo(vars)

  if (ddi$file_type == "hierarchical") {
    out <- read_ipums_hier(ddi, vars, n_max, "list", data_file, verbose, rectype_convert)
  } else if (ddi$file_type == "rectangular") {
    out <- read_ipums_rect(ddi, vars, n_max, data_file, verbose)
    warning("Assuming data rectangularized to 'P' record type")
    out <- list(P = out)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

  out <- set_ipums_df_attributes(out, ddi)
  out
}


read_ipums_hier <- function(ddi, vars, n_max, data_structure, data_file, verbose, rectype_convert) {
  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    stop("Hierarchical data cannot be read as csv.")
  }
  all_vars <- ddi$var_info

  rec_vinfo <- dplyr::filter(all_vars, .data$var_name == ddi$rectype_idvar)
  if (nrow(rec_vinfo) > 1) stop("Cannot support multiple rectype id variables.", call. = FALSE)

  all_vars <- select_var_rows(all_vars, vars)
  if (!rec_vinfo$var_name %in% all_vars$var_name) {
    if (verbose) message(paste0("Adding rectype id var '", rec_vinfo$var_name, "' to data."))
    all_vars <- dplyr::bind_rows(rec_vinfo, all_vars)
  }

  nonrec_vinfo <- dplyr::filter(all_vars, .data$var_name != ddi$rectype_idvar)
  nonrec_vinfo <- tidyr::unnest_(nonrec_vinfo, "rectypes", .drop = FALSE)

  if (verbose) cat("Reading data...\n")
  lines <- read_check_for_negative_bug(
    readr::read_lines,
    data_file,
    progress = show_readr_progress(verbose),
    n_max = n_max,
    locale = ipums_locale(ddi$file_encoding)
  )

  if (verbose) cat("Parsing data...\n")
  if (data_structure == "long") {
    nlines <- length(lines)

    # Make a data.frame with all of the variables we will need
    out <- purrr::map(all_vars$var_type, nlines = nlines, function(.x, nlines) {
      switch(
        .x,
        "numeric" = rep(NA_real_, nlines),
        "character" = rep(NA_character_, nlines)
      )
    })
    out <- purrr::set_names(out, all_vars$var_name)
    out <- tibble::as.tibble(out)

    # Add rectype var into our empty data frame
    out[seq_len(nlines), rec_vinfo$var_name] <-
      stringr::str_sub(lines, rec_vinfo$start, rec_vinfo$end)

    # Some projects have numeric RECTYPE in data, even though DDI refers to them by character.
    # This is being addressed in redmine 14283, so need to check that this conversion
    # is actually needed
    if (is.null(rectype_convert)) {
      rectype_convert <- get_proj_config(ddi$ipums_project)$rectype_trans
    }
    proj_has_conversion <- !is.null(rectype_convert)
    ddi_not_updated <- any(!names(rectype_convert) %in% ddi$rectypes)
    if (proj_has_conversion && ddi_not_updated) {
      out$RECTYPE_DDI <- convert_rectype(rectype_convert, out[[rec_vinfo$var_name]])
      rec_vinfo$var_name <- "RECTYPE_DDI"
    }

    # Add the rest of the variables
    all_rec_types <- unique(out[[rec_vinfo$var_name]])
    rec_index <- purrr::map(all_rec_types, ~out[[rec_vinfo$var_name]] == .)
    rec_index <- purrr::set_names(rec_index, all_rec_types)

    purrr::pwalk(nonrec_vinfo, function(var_name, start, end, imp_decim, var_type, rectypes, ...) {
      var_data <- stringr::str_sub(lines[rec_index[[rectypes]]], start, end)
      if (var_type == "numeric") {
        var_data <- as.numeric(var_data)
      }
      out[rec_index[[rectypes]], var_name] <<- var_data
    })

    out <- set_ipums_var_attributes(out, all_vars)
  } else if (data_structure == "list") {
    # Determine rectypes
    rec_type <- stringr::str_sub(lines, rec_vinfo$start, rec_vinfo$end)

    # Some projects have numeric RECTYPE in data, even though DDI refers to them by character.
    # This is being addressed in redmine 14283, so need to check that this conversion
    # is actually needed
    if (is.null(rectype_convert)) {
      rectype_convert <- get_proj_config(ddi$ipums_project)$rectype_trans
    }
    proj_has_conversion <- !is.null(rectype_convert)
    ddi_not_updated <- any(!names(rectype_convert) %in% ddi$rectypes)

    if (proj_has_conversion && ddi_not_updated) {
      rec_type <- convert_rectype(rectype_convert, rec_type)
    }

    rec_types_in_extract <- dplyr::intersect(rec_vinfo$rectypes[[1]], unique(rec_type))

    # Make a data.frame for each rectype
    out <- purrr::map(rec_types_in_extract, function(rt) {
      vars_in_rec <- nonrec_vinfo[purrr::map_lgl(nonrec_vinfo$rectypes, ~rt %in% .), ]
      lines_in_rec <- lines[rec_type == rt]

      nlines_rt <- length(lines_in_rec)

      out_rt <- purrr::map(vars_in_rec$var_type, nlines = nlines_rt, function(.x, nlines) {
        switch(
          .x,
          "numeric" = rep(NA_real_, nlines),
          "character" = rep(NA_character_, nlines)
        )
      })
      out_rt <- purrr::set_names(out_rt, vars_in_rec$var_name)
      out_rt <- tibble::as.tibble(out_rt)


      # Add in the variables
      purrr::pwalk(vars_in_rec, function(var_name, start, end, imp_decim, var_type, rectypes, ...) {
        var_data <- stringr::str_sub(lines_in_rec, start, end)
        if (var_type == "numeric") {
          var_data <- as.numeric(var_data)
        }
        out_rt[[var_name]] <<- var_data
      })

      out_rt <- set_ipums_var_attributes(out_rt, vars_in_rec)
    })

    # If value labels for rectype are available use them to name data.frames
    rt_lbls <- rec_vinfo$val_labels[[1]]
    matched_lbls <- match(rec_types_in_extract, rt_lbls$val)
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
    } else {
      names(out) <- rec_types_in_extract
    }


  }
  out
}

read_ipums_rect <- function(ddi, vars, n_max, data_file, verbose) {
  all_vars <- select_var_rows(ddi$var_info, vars)

  col_types <- purrr::map(all_vars$var_type, function(x) {
    if (x == "numeric") out <- readr::col_double()
    else if(x == "character") out <- readr::col_character()
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
  out <- set_ipums_var_attributes(out, all_vars)

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
