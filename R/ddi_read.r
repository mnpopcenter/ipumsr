# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport


#' Read metadata about an IPUMS extract from a DDI (.xml) file
#'
#' Reads the metadata about an IPUMS extract from a DDI file into R.
#' Includes information about variable and value labels, terms of
#' usage for the data and positions for the fixed-width file.
#'
#' @param ddi_file Filepath to DDI xml file
#' @param data_layer If ddi_file is an extract with multiple DDIs, dplyr
#'   \code{\link[dplyr]{select}}-stlye notation indicating which .xml data
#'   layer to load.
#' @examples
#' \dontrun{
#' metadata <- read_ddi("cps_00001.xml")
#' }
#' @family ipums_metadata
#' @export
read_ddi <- function(ddi_file, data_layer = NULL) {
  data_layer <- enquo(data_layer)
  if (stringr::str_sub(ddi_file, -4) == ".zip") {
    ddi_in_zip <- find_files_in_zip(ddi_file, "xml", data_layer)
    ddi_file_load <- unz(ddi_file, ddi_in_zip)
  } else {
    ddi_file_load <- ddi_file
  }
  ddi_xml <- xml2::read_xml(ddi_file_load, data_layer = NULL)

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

  if (length(var_info_xml) == 0) {
    # Empty dataframe if there's no variable info
    var_info <- tibble::data_frame(
      var_name = character(0),
      var_label = character(0),
      var_desc = character(0),
      val_label = list(),
      start = numeric(0),
      end = numeric(0),
      imp_decim = numeric(0),
      var_type = character(0),
      rectypes = logical(0)
    )
  } else {
    loc <- xml2::xml_find_first(var_info_xml, "d1:location")
    start <- as.numeric(xml2::xml_attr(loc, "StartPos"))
    end <- as.numeric(xml2::xml_attr(loc, "EndPos"))

    var_info <- dplyr::data_frame(
      var_name = xml2::xml_attr(var_info_xml, "name"),
      var_label =  xml2::xml_text(xml2::xml_find_first(var_info_xml, "d1:labl")),
      var_desc = xml2::xml_text(xml2::xml_find_first(var_info_xml, "d1:txt")),
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
      }),
      start = start,
      end = end,
      imp_decim = as.numeric(xml2::xml_attr(var_info_xml, "dcml")),
      var_type = xml2::xml_attr(xml2::xml_find_first(var_info_xml, "d1:varFormat"), "type")
    )

    if  (file_type == "hierarchical") {
      var_info$rectypes <- stringr::str_split(xml2::xml_attr(var_info_xml, "rectype"), " ")
    } else {
      var_info$rectypes <- NA
    }
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

  class(out) <- "ipums_ddi"
  out
}


#' Read metadata from a text codebook in a NHGIS or Terra area-level extract
#'
#' Read text formatted codebooks provided by some IPUMS extract systems such as
#' NHGIS and Terra Area-level extracts in a format analagous to the DDI's
#' available for other projects.
#'
#' @return
#'   A \code{ipums_ddi} object with information on the variables included in the
#'   csv file of a NHGIS extract.
#' @param cb_file Filepath to the codebook (either the .zip file directly downloaded
#'   from the webiste, or the path to the unzipped .txt file).
#' @param data_layer dplyr \code{\link[dplyr]{select}}-stlye notation for uniquely
#'   identifying the data layer to load. Required for reading from .zip files
#'    for extracts with multiple files.
#' @examples
#' \dontrun{
#' data <- read_ipums_codebook("nhgis0001_csv.zip")
#' }
#' @family ipums_metadata
#' @export
read_ipums_codebook <- function(cb_file, data_layer = NULL) {
  data_layer <- enquo(data_layer)
  if (stringr::str_sub(cb_file, -4) == ".zip") {
    cb_name <- find_files_in_zip(cb_file, "txt", multiple_ok = TRUE)
    # There are 2 formats for extracts, so we have to do some work here.
    # IPUMS Terra always(?) has 2 text files, one is a codebook for all files
    # in the extract and another with a name that ends in "info.txt" and
    # isn't useful
    if (length(cb_name) > 1) {
      # First try to get rid of the "info" txt
      cb_name <- cb_name[!stringr::str_detect(cb_name, "info\\.txt$")]

      # If we still have multiple, then we should try to use the data_layer filter
      # because we're probably in a NHGIS extract
      if (length(cb_name) > 1) cb_name <- find_files_in_zip(cb_file, "txt", data_layer)
    }
    if (length(cb_name) == 1) {
      cb <- readr::read_lines(unz(cb_file, cb_name))
    } else {
      cb <- NULL
    }
  } else {
    cb_name <- cb_file
    if (file.exists(cb_name)) {
      cb <- readr::read_lines(cb_name)
    }  else {
      cb <- NULL
    }
  }

  if (is.null(cb)) {
    stop("Could not find text codebook.")
  }
  # Section markers are a line full of dashes (setting to 5+ to eliminate false positives)
  section_markers <- which(stringr::str_detect(cb, "^[-]{5,}+$"))

  # Second line tells if it is NHGIS or IPUMS Terra codebook
  if (stringr::str_detect(cb[2], "IPUMS Terra")) type <- "Terra"
  else if (stringr::str_detect(cb[2], "NHGIS")) type <- "NHGIS"
  else stop("Unknown codebook format.")

  # Get table names (var_desc) and variable labels (var_label)
  # from data dictionary section using messy string parsing code
  dd <- find_cb_section(cb, "^Data Dictionary$", section_markers)

  if (type == "Terra") {
    data_file_rows <- which(stringr::str_detect(dd, "^Data File:"))
    data_file_sections <- purrr::map2(data_file_rows, c(data_file_rows[-1], length(dd)), ~seq(.x + 1, .y - 1))
    data_file_names <- stringr::str_match(dd[data_file_rows], "Data File: (.+)")[, 2]

    # Only get var info from file you're downloading (specfied in data_layer)
    if (quo_is_null(data_layer)) {
      this_file <- seq_along(data_file_names)
    } else {
      this_file <- which(data_file_names == dplyr::select_vars(data_file_names, !!data_layer))
    }
    if (length(this_file) > 1) {
      stop("Multiple codebooks found, please specify which to use with the `data_layer` argument")
    }
    var_info <- dd[data_file_sections[[this_file]]]
    var_info <- stringr::str_match(var_info, "([[:alnum:]|[:punct:]]+):[:blank:]+(.+)$")
    var_info <- tibble::data_frame(
      var_name = var_info[, 2],
      var_label = var_info[, 3],
      var_desc = ""
    )
    var_info <- var_info[!is.na(var_info$var_name), ]
  } else if (type == "NHGIS") {
    context_start <- which(dd == "Context Fields ") + 1
    context_end <- which(stringr::str_detect(dd, "^[:blank:]$")) - 1
    context_end <- min(context_end[context_end > context_start])
    context_rows <- seq(context_start, context_end)

    context_vars <- dd[context_rows]
    context_vars <- stringr::str_match(context_vars, "([[:alnum:]|[:punct:]]+):[:blank:]+(.+)$")
    context_vars <- tibble::data_frame(
      var_name = context_vars[, 2],
      var_label = context_vars[, 3],
      var_desc = ""
    )
    context_vars <- context_vars[!is.na(context_vars$var_name), ]

    table_name_rows <- which(stringr::str_detect(dd, "^[:blank:]*Table [0-9]+:"))
    table_sections <- purrr::map2(table_name_rows, c(table_name_rows[-1], length(dd)), ~seq(.x, .y - 1))

    table_vars <- purrr::map_df(table_sections, function(rows) {
      table_name <- stringr::str_match(dd[rows[1]], "^[:blank:]*Table [0-9]+:[:blank:]+(.+)$")[, 2]
      nhgis_table_code <- stringr::str_match(dd[rows[4]], "^[:blank:]*NHGIS code:[:blank:]+(.+)$")[, 2]
      vars <- stringr::str_match(dd[rows[-1:-4]], "([[:alnum:]|[:punct:]]+):[:blank:]+(.+)$")
      vars <- vars[!is.na(vars[, 2]), ]
      dplyr::data_frame(
        var_name = vars[, 2],
        var_label = vars[, 3],
        var_desc = paste0(table_name, " (", nhgis_table_code, ")")
      )
    })
    var_info <- dplyr::bind_rows(context_vars, table_vars)
  }

  # Get License and Condition section
  conditions_text <- find_cb_section(cb, "^Citation and Use of .+ Data", section_markers)
  conditions_text <- paste(conditions_text, collapse = "\n")


  out <- list(
    file_name = cb_name,
    file_path = "",
    file_type = "rectangular",
    rec_types = NULL,
    rectype_idvar = NULL,
    var_info = var_info,
    conditions = conditions_text,
    license = NULL
  )
  class(out) <- "ipums_ddi"
  out
}


find_cb_section <- function(cb_text, section, section_markers) {
  start <- which(stringr::str_detect(cb_text, section))
  start <- start[start - 1 %in% section_markers & start + 1 %in% section_markers] + 2

  end <- min(c(length(cb_text), section_markers[section_markers > start])) - 1
  cb_text[seq(start, end)]
}
