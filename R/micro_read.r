# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport


#' Read data from an IPUMS extract
#'
#' Reads a dataset downloaded from the IPUMS extract system.
#'
#' For IPUMS projects with microdata, it relies on a downloaded
#' DDI codebook and a fixed-width file. Loads the data with
#' value labels (using \code{\link[haven]{labelled}} format)
#' and variable labels.
#'
#' @param ddi Either a filepath to a DDI xml file downloaded from
#'   the website, or a \code{ipums_ddi} object parsed by \code{\link{read_ddi}}
#' @param vars A vector of variable names using \code{\link[dplyr]{select}}-style
#'   convetions. For hierarchical data, the rectype id variable will be added even
#'   if it is not specified.
#' @param n_max The maximum number of records to load.
#' @param data_structure For hierarchical data extract, one of "long", "list",
#'   or "nested" to indicate how to structure the data once loaded. "long" data
#'   puts all rectypes in the same data.frame, with \code{NA} values for
#'   variables that do not apply to the rectype. "list" data puts a data.frame
#'   for each rectype into a list. "nested" creates a single data.frame with one
#'   row per most-general rectype, with nested data.frames
#'   that have information for each rectype.
#' @param data_file Specify a directory to look for the data file.
#'   If left empty, it will look in the same directory as the DDI file.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @examples
#' \dontrun{
#' data <- read_micro("cps_00001.xml")
#' }
#' @family ipums_read
#' @export
read_ipums_micro <- function(
  ddi,
  vars = NULL,
  n_max = -1,
  data_structure = c("long", "list", "nested"),
  data_file = NULL,
  verbose = TRUE
) {
  if (is.character(ddi)) ddi <- read_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)
  if (!file.exists(data_file) & file.exists(paste0(data_file, ".gz"))) {
    data_file <- paste0(data_file, ".gz")
  }
  if (verbose) {
    cat(ddi$conditions)
    cat("\n\n")
    cat(ddi$citation)
    cat("\n\n")
  }

  vars <- enquo(vars)
  data_structure <- match.arg(data_structure)

  if (ddi$file_type == "hierarchical") {
    read_ipums_hier(ddi, vars, n_max, data_structure, data_file, verbose)
  } else if (ddi$file_type == "rectangular") {
    read_ipums_rect(ddi, vars, n_max, data_file, verbose)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

}

read_ipums_hier <- function(ddi, vars, n_max, data_structure, data_file, verbose) {
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
  lines <- readr::read_lines(data_file, progress = FALSE, n_max = n_max)

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
  } else if (data_structure == "list" | data_structure == "nested") {
    # Determine rectypes
    rec_type <- stringr::str_sub(lines, rec_vinfo$start, rec_vinfo$end)
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
    names(out) <- rec_types_in_extract

    if (data_structure == "nested") {
      # Work backwards in rectypes to start with the most nested
      rectypes_to_nest <- rev(rec_types_in_extract[-1])

      for (rt in rectypes_to_nest) {
        next_rt <- rec_types_in_extract[which(rec_types_in_extract == rt) - 1]

        merge_vars <- dplyr::intersect(names(out[[rt]]), names(out[[next_rt]]))
        nest_vars <- dplyr::setdiff(names(out[[rt]]), merge_vars)

        out[[rt]] <- tidyr::nest_(out[[rt]], rt, nest_vars)
        out[[next_rt]] <- dplyr::full_join(out[[next_rt]], out[[rt]], by = merge_vars)
        out[[rt]] <- NULL
      }
      out <- out[[rec_types_in_extract[1]]]
    }
  }
  out
}

read_ipums_rect <- function(ddi, vars, n_max, data_file, verbose) {
  all_vars <- select_var_rows(ddi$var_info, vars)

  col_types <- dplyr::case_when(
    all_vars$var_type == "numeric" ~ "d",
    all_vars$var_type == "character" ~ "c"
  )
  col_types <- paste(col_types, collapse = "")

  col_positions <- readr::fwf_positions(
    start = all_vars$start,
    end = all_vars$end,
    col_names = all_vars$var_name
  )

  out <- readr::read_fwf(data_file, col_positions, col_types, n_max = n_max)
  out <- set_ipums_var_attributes(out, all_vars)

  out
}
