# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr


#' Read metadata about an IPUMS extract from a DDI (.xml) file
#'
#' Reads the metadata about an IPUMS extract from a DDI file into R.
#' Includes information about variable and value labels, terms of
#' usage for the data and positions for the fixed-width file.
#'
#' @param ddi_file Filepath to DDI xml file
#' @param data_layer If ddi_file is an extract with multiple DDIs, dplyr
#'   \code{\link[dplyr]{select}}-style notation indicating which .xml data
#'   layer to load.
#' @param lower_vars Logical indicating whether to convert variable names
#'   to lowercase (default is FALSE, in line with IPUMS conventions)
#' @return An \code{ipums_ddi} object with metadata information.
#' @examples
#' # Example extract DDI
#' ddi_file <- ipums_example("cps_00006.xml")
#' ddi <- read_ipums_ddi(ddi_file)
#' @family ipums_metadata
#' @export
read_ipums_ddi <- function(ddi_file, data_layer = NULL, lower_vars = FALSE) {
  data_layer <- enquo(data_layer)

  custom_check_file_exists(ddi_file)

  if (fostr_sub(ddi_file, -4) == ".zip") {
    ddi_in_zip <- find_files_in(ddi_file, "xml", data_layer)
    ddi_file_load <- unz(ddi_file, ddi_in_zip)
  } else {
    ddi_file_load <- ddi_file
  }
  ddi_xml <- xml2::read_xml(ddi_file_load, data_layer = NULL)

  # Basic information
  conditions <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:dataAccs/d1:useStmt/d1:conditions"
  )

  citation <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:dataAccs/d1:useStmt/d1:citReq"
  )

  ipums_project <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:citation/d1:serStmt/d1:serName"
  )

  extract_notes <- xml_text_from_path_collapsed(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:notes"
  )

  extract_date <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:stdyDscr/d1:citation/d1:prodStmt/d1:prodDate/@date"
  )
  extract_date <- as.Date(extract_date)

  # File information
  files <- xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:fileDscr")
  if (length(files) > 1) {
    warning("Extracts with multiple files not supported, using first file.", call. = FALSE)
  }

  file_name <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileName"
  )

  file_type <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/@type"
  )

  file_encoding <- xml_text_from_path_first(
    ddi_xml,
    "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileType/@charset"
  )

  # Get rectype info if hierarchical
  if (file_type == "hierarchical") {
    rectypes <- xml_text_from_path_all(
      ddi_xml,
      "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp/@rectype"
    )

    rectype_idvar <- xml_text_from_path_all(
      ddi_xml,
      "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp/@recidvar"
    )
    rectype_idvar <- rectype_idvar[length(rectype_idvar)]

    rectypes_keyvars <- xml2::xml_text(
      xml2::xml_find_first(
        xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp"),
        "@keyvar"
      )
    )
    rectypes_keyvars <- fostr_split(rectypes_keyvars, "[[:blank:]]+")
    rectypes_keyvars <- purrr::map(rectypes_keyvars, ~.[!is.na(.)])
    rectypes_keyvars <- tibble::tibble(
      rectype = rectypes,
      keyvars = rectypes_keyvars
    )

    # For some reason our extract engine can't provide value labels for rec types
    # So get it from file structure area
    rt_lbls <- xml_text_from_path_all(
      ddi_xml,
      "/d1:codeBook/d1:fileDscr/d1:fileTxt/d1:fileStrc/d1:recGrp/d1:labl"
    )
    rectype_labels <- tibble::tibble(
      val = rectypes,
      lbl = rt_lbls
    )
  } else {
    rectypes <- NULL
    rectype_idvar <- NULL
    rectype_labels <- NULL
    rectypes_keyvars <- NULL
  }

  # Get variable specific information
  var_info <- get_var_info_from_ddi(ddi_xml, file_type, rectype_idvar, rectype_labels)

  if (lower_vars) {
    var_info$var_name <- tolower(var_info$var_name)
    if (!is.null(rectype_idvar)) rectype_idvar <- tolower(rectype_idvar)
    if (!is.null(rectypes_keyvars)) {
      rectypes_keyvars$keyvars <- purrr::map(rectypes_keyvars$keyvars, tolower)
    }
  }

  make_ddi_from_scratch(
    file_name = file_name,
    file_path = dirname(ddi_file),
    file_type = file_type,
    ipums_project = ipums_project,
    extract_date = extract_date,
    extract_notes = extract_notes,
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    rectypes_keyvars = rectypes_keyvars,
    var_info = var_info,
    conditions = conditions,
    citation = citation,
    file_encoding = file_encoding
  )
}

xml_text_from_path_first <- function(xml, path) {
  xml2::xml_text(xml2::xml_find_first(xml, path))
}

xml_text_from_path_collapsed <- function(xml, path, collapse = "\n\n") {
  out <- xml2::xml_text(xml2::xml_find_all(xml, path))
  paste(out, collapse = collapse)
}

xml_text_from_path_all <- function(xml, path) {
  xml2::xml_text(xml2::xml_find_all(xml, path))
}

get_var_info_from_ddi <- function(ddi_xml, file_type, rt_idvar, rectype_labels) {
  var_info_xml <- xml2::xml_find_all(ddi_xml, "/d1:codeBook/d1:dataDscr/d1:var")
  if (length(var_info_xml) == 0) return(NULL)

  var_name <- xml2::xml_attr(var_info_xml, "name")
  start <- as.numeric(xml_text_from_path_first(var_info_xml, "d1:location/@StartPos"))
  end <- as.numeric(xml_text_from_path_first(var_info_xml, "d1:location/@EndPos"))
  width <- as.numeric(xml_text_from_path_first(var_info_xml, "d1:location/@width"))
  var_label <- xml_text_from_path_first(var_info_xml, "d1:labl")
  var_desc <- xml_text_from_path_first(var_info_xml, "d1:txt")
  imp_decim <- as.numeric(xml2::xml_attr(var_info_xml, "dcml"))

  var_type <- xml_text_from_path_first(var_info_xml, "d1:varFormat/@type")
  var_intrvl <- xml2::xml_attr(var_info_xml, "intrvl")
  var_type <- dplyr::case_when(
    var_type == "numeric" & var_intrvl == "discrete" & (width < 10) ~ "integer",
    var_type == "numeric" ~ "numeric",
    var_type == "character" ~ "character",
    TRUE ~ "character" # Default to character if it's unexpected
  )

  code_instr <- xml_text_from_path_first(var_info_xml, "d1:codInstr")

  if  (file_type == "hierarchical") {
    rectype_by_var <- fostr_split(xml2::xml_attr(var_info_xml, "rectype"), " ")
  } else {
    rectype_by_var <- NA
  }

  # Value labels
  # Some come from parsed code sections
  lbls_from_code_instr <- parse_labels_from_code_instr(code_instr, var_type)

  # For hierarchical, RECTYPE comes from elsewhere in the DDI
  if (file_type == "hierarchical") {
    # If var is numeric, need to convert
    rt_type <- var_type[var_name == rt_idvar]
    if (length(rt_type) == 1 && rt_type %in% c("numeric", "integer")) {
      rectype_labels$val <- suppressWarnings(as.numeric(rectype_labels$val))
    }
    rectype_labels <- dplyr::filter(rectype_labels, !is.na(.data$val))
    rectype_labels <- dplyr::arrange(rectype_labels, .data$val)
    # Replace in the code_instructions
    if (nrow(rectype_labels) > 0) {
      lbls_from_code_instr[[which(var_name == rt_idvar)]] <- rectype_labels
    }
  }

  val_labels <- purrr::pmap(
    list(var_info_xml, var_type, lbls_from_code_instr),
    function(vvv, vtype, extra_labels) {
      lbls <- xml2::xml_find_all(vvv, "d1:catgry")
      if (length(lbls) == 0) return(extra_labels)

      lbls <- tibble::tibble(
        val = xml_text_from_path_all(lbls, "d1:catValu"),
        lbl = xml_text_from_path_all(lbls, "d1:labl")
      )

      if (vtype %in% c("numeric", "integer")) lbls$val <- as.numeric(lbls$val)

      # Drop labels that are the same as the value
      # But leading 0's can be ignored if numeric
      if (vtype %in% c("numeric", "integer")) {
        lnum <- suppressWarnings(as.numeric(lbls$lbl))
        lbls <- dplyr::filter(lbls, (is.na(lnum) | .data$val != lnum))
      } else {
        lbls <- dplyr::filter(lbls, .data$val != .data$lbl)
      }

      out <- dplyr::bind_rows(lbls, extra_labels)
      dplyr::arrange(out, .data$val)
    })

  make_var_info_from_scratch(
    var_name = var_name,
    var_label =  var_label,
    var_desc = var_desc,
    val_labels = val_labels,
    code_instr = code_instr,
    start = start,
    end = end,
    imp_decim = imp_decim,
    var_type = var_type,
    rectypes = rectype_by_var
  )
}


#' Read metadata from a text codebook in a NHGIS or Terra area-level extract
#'
#' Read text formatted codebooks provided by some IPUMS extract systems such as
#' NHGIS and Terra Area-level extracts in a format analogous to the DDIs
#' available for other projects.
#'
#' @return
#'   A \code{ipums_ddi} object with information on the variables included in the
#'   csv file of a NHGIS extract.
#' @param cb_file Filepath to the codebook (either the .zip file directly downloaded
#'   from the website, or the path to the unzipped .txt file).
#' @param data_layer dplyr \code{\link[dplyr]{select}}-style notation for uniquely
#'   identifying the data layer to load. Required for reading from .zip files
#'    for extracts with multiple files.
#' @examples
#' # Example NHGIS extract
#' nhgis_file <- ipums_example("nhgis0008_csv.zip")
#' ddi <- read_ipums_codebook(nhgis_file)
#' @family ipums_metadata
#' @export
read_ipums_codebook <- function(cb_file, data_layer = NULL) {
  data_layer <- enquo(data_layer)
  if (path_is_zip_or_dir(cb_file)) {
    cb_name <- find_files_in(cb_file, "txt", multiple_ok = TRUE)
    # There are 2 formats for extracts, so we have to do some work here.
    # IPUMS Terra always(?) has 2 text files, one is a codebook for all files
    # in the extract and another with a name that ends in "info.txt" and
    # isn't useful
    if (length(cb_name) > 1) {
      # First try to get rid of the "info" txt
      cb_name <- cb_name[!fostr_detect(cb_name, "info\\.txt$")]

      # If we still have multiple, then we should try to use the data_layer filter
      # because we're probably in a NHGIS extract
      if (length(cb_name) > 1) cb_name <- find_files_in(cb_file, "txt", data_layer)
    }
    if (length(cb_name) == 1) {
      if (file_is_zip(cb_file)) {
        cb <- readr::read_lines(unz(cb_file, cb_name))
      } else {
        cb <- readr::read_lines(file.path(cb_file, cb_name))
      }
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
  section_markers <- which(fostr_detect(cb, "^[-]{5,}$"))

  # Second line tells if it is NHGIS or IPUMS Terra codebook
  if (fostr_detect(cb[2], "IPUMS Terra")) type <- "IPUMS Terra"
  else if (fostr_detect(cb[2], "NHGIS")) type <- "NHGIS"
  else stop("Unknown codebook format.")

  # Get table names (var_desc) and variable labels (var_label)
  # from data dictionary section using messy string parsing code
  dd <- find_cb_section(cb, "^Data Dictionary$", section_markers)

  if (type == "IPUMS Terra") {
    data_file_rows <- which(fostr_detect(dd, "^Data File:"))
    data_file_sections <- purrr::map2(data_file_rows, c(data_file_rows[-1], length(dd)), ~seq(.x + 1, .y - 1))
    data_file_names <- fostr_named_capture_single(
      dd[data_file_rows],
      "Data File: (?<data_file>.+)"
    )

    # Only get var info from file you're downloading (specfied in data_layer)
    if (quo_is_null(data_layer)) {
      this_file <- seq_along(data_file_names)
    } else {
      this_file <- which(data_file_names == tidyselect::vars_select(data_file_names, !!data_layer))
    }
    if (length(this_file) > 1) {
      stop(custom_format_text(
        "Multiple codebooks found, please specify which to use with the",
        "`data_layer` argument", indent = 2, exdent = 2
      ))
    }
    var_info <- dd[data_file_sections[[this_file]]]
    var_info <- fostr_named_capture(
      var_info,
      "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
      only_matches = TRUE
    )

    var_info <- make_var_info_from_scratch(
      var_name = var_info$var_name,
      var_label = var_info$var_label,
      var_desc = ""
    )
  } else if (type == "NHGIS") {
    # Check if file is a time series file
    time_series_type <- fostr_named_capture_single(
      cb,
      "^Time series layout:[[:blank:]]+(?<ts_type>.+)$",
      only_matches = TRUE
    )
    if (length(time_series_type) > 0) {
      is_time_series <- TRUE
    } else {
      is_time_series <- FALSE
    }

    context_start <- which(dd == "Context Fields ") + 1
    context_end <- which(fostr_detect(dd, "^[[:blank:]]$")) - 1
    context_end <- min(context_end[context_end > context_start])
    context_rows <- seq(context_start, context_end)

    context_vars <- dd[context_rows]
    context_vars <- fostr_named_capture(
      context_vars,
      "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$"
    )
    context_vars$var_desc <- ""
    context_vars <- context_vars[!is.na(context_vars$var_name), ]

    table_name_rows <- which(fostr_detect(dd, "^[[:blank:]]*Table [0-9]+:"))
    table_sections <- purrr::map2(table_name_rows, c(table_name_rows[-1], length(dd)), ~seq(.x, .y - 1))

    table_vars <- purrr::map_df(table_sections, function(rows) {
      if (is_time_series) {
        table_name_and_code <- fostr_named_capture(
          dd[rows[1]],
          "^[[:blank:]]*Table .+?:[[:blank:]]+\\((?<table_code>.+?)\\)[[:blank:]]+(?<table_name>.+)$"
        )
        nhgis_table_code <- table_name_and_code$table_code
        table_name <- table_name_and_code$table_name

        time_series_headers <- fostr_detect(dd[rows], "^[[:blank:]]+Time series")
        vars <- dd[rows][!time_series_headers]
        vars <- vars[-1] # First row was table name/code
        vars <- fostr_named_capture(
          vars,
          "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
          only_matches = TRUE
        )
        vars$var_desc <- paste0(table_name, " (", nhgis_table_code, ")")
      } else {
        table_name <- fostr_named_capture_single(
          dd[rows[1]],
          "^[[:blank:]]*Table .+?:[[:blank:]]+(?<table_name>.+)$"
        )
        nhgis_table_code <- fostr_named_capture_single(
          dd[rows[4]],
          "^[[:blank:]]*NHGIS code:[[:blank:]]+(?<table_code>.+)$"
        )
        vars <- fostr_named_capture(
          dd[rows[-1:-4]],
          "(?<var_name>[[:alnum:]|[:punct:]]+):[[:blank:]]+(?<var_label>.+)$",
          only_matches = TRUE
        )
        vars$var_desc <- paste0(table_name, " (", nhgis_table_code, ")")
      }
      vars
    })
    var_info <- make_var_info_from_scratch(
      var_name = c(context_vars$var_name, table_vars$var_name),
      var_label = c(context_vars$var_label, table_vars$var_label),
      var_desc = c(context_vars$var_desc, table_vars$var_desc)
    )
  }

  # Get License and Condition section
  conditions_text <- find_cb_section(cb, "^Citation and Use of .+ Data", section_markers)
  conditions_text <- paste(conditions_text, collapse = "\n")


  out <- make_ddi_from_scratch(
    file_name = cb_name,
    file_type = "rectangular",
    ipums_project = type,
    var_info = var_info,
    conditions = conditions_text
  )

  out
}


find_cb_section <- function(cb_text, section, section_markers) {
  start <- which(fostr_detect(cb_text, section))
  start <- start[start - 1 %in% section_markers & start + 1 %in% section_markers] + 2

  end <- min(c(length(cb_text), section_markers[section_markers > start])) - 1
  cb_text[seq(start, end)]
}

path_is_zip_or_dir <- function(file) {
  ext <- tools::file_ext(file)

  ext == "zip" || ext == ""
}

#' Create DDI structure (for internal use)
#'
#' Helper to make a new DDI structure (not very useful for end-users).
#'
#' @keywords internal
#' @export
make_ddi_from_scratch <- function(
  file_name = NULL,
  file_path = NULL,
  file_type = NULL,
  ipums_project = NULL,
  extract_date = NULL,
  extract_notes = NULL,
  rectypes = NULL,
  rectype_idvar = NULL,
  rectypes_keyvars = NULL,
  var_info = NULL,
  conditions = NULL,
  citation = NULL,
  file_encoding = NULL
) {
  out <- list(
    file_name = file_name,
    file_path = file_path,
    file_type = file_type,
    ipums_project = ipums_project,
    extract_date = extract_date,
    extract_notes = extract_notes,
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    rectypes_keyvars = rectypes_keyvars,
    var_info = var_info,
    conditions = conditions,
    citation = citation,
    file_encoding = file_encoding
  )

  class(out) <- "ipums_ddi"
  out
}

make_var_info_from_scratch <- function(
  var_name = "",
  var_label = "",
  var_desc = "",
  val_labels = list(tibble::tibble(val = numeric(0), lbl = character(0))),
  code_instr = "",
  start = NA,
  end = NA,
  imp_decim = 0,
  var_type = "",
  rectypes = NA
) {
  tibble::tibble(
    var_name = var_name,
    var_label = var_label,
    var_desc = var_desc,
    val_labels = val_labels,
    code_instr = code_instr,
    start = start,
    end = end,
    imp_decim = imp_decim,
    var_type = var_type,
    rectypes = rectypes
  )
}

make_empty_labels <- function(vt) {
  tibble::tibble(
    val = if (vt == "character") character(0) else numeric(0),
    lbl = character(0)
  )
}

# Helper to get labels out of free text from codInstr in xml
parse_labels_from_code_instr <- function(code, var_type) {
  purrr::map2(code, var_type, function(x, vt) {
    if (is.na(x)) return(make_empty_labels(vt))
    lines <- fostr_split(x, "\n")[[1]]
    labels <- parse_code_regex(lines, vt)
    dplyr::arrange(labels, .data$val)
  })
}

parse_code_regex <- function(x, vtype) {
  if (vtype %in% c("numeric", "integer")) {
    labels <- fostr_named_capture(
      x,
      "^(?<val>-?[0-9.,]+)(([[:blank:]][[:punct:]]|[[:punct:]][[:blank:]]|[[:blank:]]|=)+)(?<lbl>.+?)$",
      only_matches = TRUE
    )

    labels$val <- as.numeric(fostr_replace_all(labels$val, ",", ""))
  } else {
    labels <- fostr_named_capture(
      x,
      "^(?<val>[[:graph:]]+)(([[:blank:]]+[[:punct:]|=]+[[:blank:]])+)(?<lbl>.+)$",
      only_matches = TRUE
    )

  }
  labels
}
