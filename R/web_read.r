#' Read metadata about an IPUMS extract
#'
#' Reads the metadata about an IPUMS extract from a DDI file into R.
#' Includes information about variable and value labels, terms of
#' usage for the data and positions for the fixed-width file.
#'
#' @param ddi_file Filepath to DDI xml file
#' @examples
#' \dontrun{
#' metadata <- ip_read_ddi("cps_00001.xml")
#' }
#' @seealso ip_read_data
#' @export
ip_read_ddi <- function(ddi_file) {
  ddi_xml <- xml2::read_xml(ddi_file)

  # Print citation/conditions
  cite_info <-  xml2::xml_find_all(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:dataAccs/d1:useStmt"
  )

  conditions <- xml2::xml_find_all(cite_info, "d1:conditions")
  conditions <- xml2::xml_text(conditions)

  citation <- xml2::xml_find_all(cite_info, "d1:citReq")
  citation <- xml2::xml_text(citation)


  # Files
  files <- xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:fileDscr")

  if (length(files) > 1) stop("Extracts with multiple files not supported.", call. = FALSE)

  file_name <- xml2::xml_find_all(files, "d1:fileTxt/d1:fileName")
  file_name <- xml2::xml_text(file_name)

  file_structure <- xml2::xml_find_all(files, "d1:fileTxt/d1:fileStrc")

  file_type <- xml2::xml_attr(file_structure, "type")

  # Get rectype info if hierarchical
  if (file_type == "hierarchical") {
    rectypes <- xml2::xml_find_all(file_structure, "d1:recGrp")
    rectypes <- xml2::xml_attr(rectypes, "rectype")

    rectype_idvar <- xml2::xml_find_all(file_structure, "d1:recGrp")
    rectype_idvar <- xml2::xml_attr(rectype_idvar, "recidvar")

    # TODO: Figure out system for rectype vars
    # For cps, the last one is the actual one. The first one has a "P" at the end
    # for Person level rectype var...
    rectype_idvar <- rectype_idvar[length(rectype_idvar)]
  } else {
    rectypes <- NULL
    rectype_idvar <- NULL
  }

  var_info_xml <- xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:dataDscr/d1:var")

  loc <- xml2::xml_find_first(var_info_xml, "d1:location")
  start <- as.numeric(xml2::xml_attr(loc, "StartPos"))
  end <- as.numeric(xml2::xml_attr(loc, "EndPos"))

  var_info <- dplyr::data_frame(
    var_name = xml2::xml_attr(var_info_xml, "name"),
    start = start,
    end = end,
    var_label =  xml2::xml_text(xml2::xml_find_first(var_info_xml, "d1:labl")),
    var_label_long = xml2::xml_text(xml2::xml_find_first(var_info_xml, "d1:txt")),
    imp_decim = as.numeric(xml2::xml_attr(var_info_xml, "dcml")),
    var_type = xml2::xml_attr(xml2::xml_find_first(var_info_xml, "d1:varFormat"), "type"),
    val_label = purrr::map(var_info_xml, function(vvv, vtype) {
      lbls <- xml2::xml_find_all(vvv, "d1:catgry")
      if (length(lbls) == 0) return(dplyr::data_frame(val = numeric(0), lbl = character(0)))

      lbls <- dplyr::data_frame(
        val = xml2::xml_text(xml2::xml_find_all(lbls, "d1:catValu")),
        lbl = xml2::xml_text(xml2::xml_find_all(lbls, "d1:labl"))
      )

      vtype <- xml2::xml_attr(xml2::xml_find_first(vvv, "d1:varFormat"), "type")
      if (vtype == "numeric") lbls$val <- as.numeric(lbls$val)
      lbls
    })
  )

  if  (file_type == "hierarchical") {
    var_info$rectypes <- stringr::str_split(xml2::xml_attr(var_info_xml, "rectype"), " ")
  } else {
    var_info$rectypes <- NA
  }

  out <- list(
    file_name = file_name,
    file_path = dirname(ddi_file),
    file_type = file_type,
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    var_info = var_info,
    conditions = conditions,
    citation = citation
  )

  class(out) <- "ip_ddi"
  out
}

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
#'   the website, or a ip_ddi object parsed by \code{\link{ip_read_ddi}}
#' @param vars A vector of variable names using \code{\link[dplyr]{select}}-style
#'   convetions. For hierarchical data, the rectype id variable will be added even
#'   if it is not specified.
#' @param n_max The maximum number of records to load.
#' @param data_file Specify a directory to look for the data file.
#'   If left empty, it will look in the same directory as the DDI file.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @examples
#' \dontrun{
#' data <- ip_read_data("cps_00001.xml")
#' }
#' @seealso ip_read_ddi
#' @export
ip_read_data <- function(ddi, vars = NULL, n_max = Inf, data_file = NULL, verbose = TRUE) {
  if (is.character(ddi)) ddi <- ip_read_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)
  if (verbose) {
    cat(ddi$conditions)
    cat("\n\n")
    cat(ddi$citation)
    cat("\n\n")
  }

  vars <- enquo(vars)

  if (ddi$file_type == "hierarchical") {
    ip_read_hier(ddi, vars, n_max, data_file, verbose)
  } else if (ddi$file_type == "rectangular") {
    ip_read_rect(ddi, vars, n_max, data_file, verbose)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

}

ip_read_hier <- function(ddi, vars, n_max, data_file, verbose) {
  all_vars <- ddi$var_info

  rec_vinfo <- dplyr::filter(all_vars, .data$var_name == ddi$rectype_idvar)
  if (!quo_is_null(vars)) {
    vars <- dplyr::select_vars(all_vars$var_name, !!!vars)
    if (!rec_vinfo$var_name %in% vars) {
      if (verbose) message(paste0("Adding rectype id var '", rec_vinfo$var_name, "' to data."))
      vars <- c(rec_vinfo$var_name, vars)

    }
    all_vars <- dplyr::filter(all_vars, .data$var_name %in% vars)
  }



  if (nrow(rec_vinfo) > 1) stop("Cannot support multiple rectype id variables.", call. = FALSE)
  nonrec_vinfo <- dplyr::filter(all_vars, .data$var_name != ddi$rectype_idvar)
  nonrec_vinfo <- tidyr::unnest_(nonrec_vinfo, "rectypes", .drop = FALSE)

  if (verbose) cat("Reading data...\n")
  lines <- readr::read_lines(data_file, progress = FALSE, n_max = n_max)

  if (verbose) cat("Parsing data...\n")
  nlines <- length(lines)

  # Make a data.frame with all of the variables we will need
  out <- purrr::map(all_vars$var_type, nlines = nlines, function(.x, nlines) {
    switch(
      .x,
      "numeric" = rep(NA_real_, nlines),
      "character" = rep(NA_character_, nlines)
      )
  })
  out <- purrr::map2(out, all_vars$val_label, function(var, lbls) {
      if (nrow(lbls) == 0) return(var)
      haven::labelled(var, purrr::set_names(lbls$val, lbls$lbl))
    })
  out <- purrr::map2(out, all_vars$var_label, ~rlang::set_attrs(.x, label = .y))
  out <- purrr::map2(out, all_vars$var_label_long, ~rlang::set_attrs(.x, label_long = .y))
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
      var_data <- var_data / (10 ^ imp_decim)
    }
    out[rec_index[[rectypes]], var_name] <<- var_data
  })

  out
}

ip_read_rect <- function(ddi, vars, n_max, data_file, verbose) {
  all_vars <- ddi$var_info

  if (!quo_is_null(vars)) {
    vars <- dplyr::select_vars(all_vars$var_name, !!!vars)
    all_vars <- dplyr::filter(all_vars, .data$var_name %in% vars)
  }

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

  purrr::pwalk(
    all_vars,
    function(var_name, imp_decim, var_label, var_label_long, val_label, ...) {
      if (imp_decim > 0) out[[var_name]] <<- out[[var_name]] / (10 ^ imp_decim)
      if (nrow(val_label) > 0) {
        out[[var_name]] <<- haven::labelled(
          out[[var_name]],
          purrr::set_names(val_label$val, val_label$lbl)
        )
      }
      out[[var_name]] <<- rlang::set_attrs(
        out[[var_name]],
        label = var_label,
        label_long = var_label_long
      )
    }
  )

  out
}
