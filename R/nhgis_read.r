#' Read data from an NHGIS extract
#'
#' Reads a dataset downloaded from the NHGIS extract system. Relies on csv files
#' (with or without the extra header row).
#'
#' @return
#'   Either a \code{tbl_df} with only the tabular data, or if a \code{shape_file} is
#'   specified, a \code{\link[sf]{sf}} object with the tabular data and polygons.
#' @param data_file Filepath to the data (either the .zip file directly downloaded
#'   from the webiste, or the path to the unzipped .csv file).
#' @param shape_file (Optional) filepath to the shape files (either the .zip
#'   file directly downloaded from the webiste, or the path to the unzipped
#'   files).
#' @param data_layer A regular expression uniquely identifying the data layer to
#'   load. Required for reading from .zip files for extracts with multiple files.
#' @param shape_layer A regular expression uniquely identifying the shape layer to
#'   load. Required for reading from .zip files for extracts with multiple files.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @examples
#' \dontrun{
#' data <- read_nhgis("nhgis0001_csv.zip", "nhgis0001_shp.zip")
#' }
#' @family ipums_read
#' @export
read_nhgis <- function(
  data_file,
  shape_file = NULL,
  data_layer = NULL,
  shape_layer = NULL,
  verbose = TRUE
) {
  # Read data files ----
  data_is_zip <- stringr::str_sub(data_file, -4) == ".zip"
  if (data_is_zip) {
    csv_name <- find_files_in_zip(data_file, "csv", data_layer)
    cb_ddi_info <- try(read_nhgis_codebook(data_file, data_layer), silent = TRUE)
  } else {
    cb_name <- stringr::str_replace(data_file, "\\.txt$", "_codebook\\.txt")
    cb_ddi_info <- try(read_nhgis_codebook(cb_name), silent = TRUE)
  }
  #
  if (class(cb_ddi_info) == "try-error") {
    cb_ddi_info <- list(
      file_name = NULL,
      file_path = NULL,
      file_type = "rectangular",
      rec_types = NULL,
      rectype_idvar = NULL,
      var_info = NULL,
      conditions = paste0(
        "Use of NHGIS data is subject to conditions, including that ",
        "publications and research which employ NHGIS data should cite it",
        "appropiately. Please see www.nhgis.org for more information."
      ),
      license = NULL
    )
  }

  if (verbose) cat(cb_ddi_info$conditions)

  # Read data
  if (verbose) cat("\n\nReading data file...\n")
  if (data_is_zip) {
    data <- readr::read_csv(unz(data_file, csv_name), col_types = readr::cols(.default = "c"))
  } else {
    data <- readr::read_csv(data_file, col_types = readr::cols(.default = "c"))
  }

  # If extract is NHGIS's "enhanced" csvs with an extra header row,
  # then remove the first row.
  # (determine by checking if the first row is entirely character
  # values that can't be converted to numeric)
  first_row <- readr::type_convert(data[1, ], col_types = readr::cols())
  first_row_char <- purrr::map_lgl(first_row, rlang::is_character)
  if (all(first_row_char)) data <- data[-1, ]

  data <- readr::type_convert(data, col_types = readr::cols())

  # Read shape files (if they exist) ----
  if (!is.null(shape_file)) {
    if (verbose) cat("Reading geography...\n")

    shape_found <- FALSE
    # Case 1: Shape file specified is a .zip file
    shape_is_zip <- stringr::str_sub(shape_file, -4) == ".zip"
    if (shape_is_zip) {
      shape_file_names <- utils::unzip(shape_file, list = TRUE)$Name


      # Case 1a: First zip file has zip files of shape files within it
      zip_in_zip <- stringr::str_sub(shape_file_names, -4) == ".zip"
      if (any(zip_in_zip)) {
        shape_zips <- shape_file_names[zip_in_zip]
        if (!is.null(shape_layer)) {
          shape_zips <- stringr::str_subset(shape_zips, shape_layer)
        }
        if (length(shape_zips) == 1) {
          shape_temp <- tempfile()
          dir.create(shape_temp)
          utils::unzip(shape_file, shape_zips, exdir = shape_temp)
          utils::unzip(file.path(shape_temp, shape_zips), exdir = shape_temp)

          read_shape_file <- file.path(shape_temp, dir(shape_temp, "\\.shp$"))
          shape_found <- TRUE
        } else if (length(shape_zips) > 1) {
          stop(paste0(
            "Multiple shape files found, please use the `shape_layer` argument to ",
            " specify which layer you want to load.\n", paste(shape_zips, collapse = ", ")
          ), .call = FALSE)
        }

      }
      # Case 1b: First zip file has .shp files within it
      shp_in_zip <- stringr::str_sub(shape_file_names, -4) == ".shp"
      if (any(shp_in_zip)) {
        shape_shps <- shape_file_names[shp_in_zip]
        if (!is.null(shape_layer)) {
          shape_shps <- stringr::str_subset(shape_shps, shape_layer)
        }

        if (length(shape_shps) == 1) {
          shape_shp_files <- paste0(
            stringr::str_sub(shape_shps, 1, -4),
            c("dbf", "prj", "sbn", "sbx", "shp", "shp.xml", "shx")
          )
          shape_temp <- tempfile()
          dir.create(shape_temp)
          utils::unzip(shape_file, shape_shp_files, exdir = shape_temp)

          read_shape_file <- file.path(shape_temp, shape_shps)
          shape_found <- TRUE
        } else if (length(shape_shps) > 1) {
          stop(paste0(
            "Multiple shape files found, please use the `shape_layer` argument to ",
            " specify which layer you want to load.\n", paste(shape_shps, collapse = ", ")
          ), .call = FALSE)
        }
      }
      if (!shape_found) {
        stop("Zip file not formatted as expected. Please unzip and try again.")
      }
    }

    # Case 2: Shape file specified is a .shp file
    shape_is_shp <- !shape_found & stringr::str_sub(shape_file, -4) == ".shp"
    if (shape_is_shp) {
      shape_found <- TRUE
      read_shape_file <- shape_file
    }

    if (!shape_is_zip & !shape_is_shp) {
      stop("Expected shape file to be a .zip or .shp file.")
    }

    sf_data <- sf::read_sf(read_shape_file)

    # Only join on vars that are in both and are called "GISJOIN*"
    join_vars <- intersect(names(data), names(sf_data))
    join_vars <- stringr::str_subset(join_vars, "GISJOIN*")

    # Drop the overlapping columns from the shape file
    data <- dplyr::full_join(sf_data, data, by = join_vars, suffix = c("_shape", ""))
    data <- dplyr::select(data, -dplyr::ends_with("_shape"))
    data <- sf::st_as_sf(tibble::as_tibble(data))
  }

  # Add variable labels
  purrr::pwalk(cb_ddi_info$var_info, function(var_name, var_label, var_label_long,...) {
    attr(data[[var_name]], "label") <<- var_label
    attr(data[[var_name]], "label_long") <<- var_label_long
  })

  data
}

#' Read metadata from a text codebook in a NHGIS extract
#'
#' Reads a codebook downloaded from the NHGIS extract system and formats it
#' analagously to metadata read from a DDI.
#'
#' @return
#'   A \code{ip_ddi} object with information on the variables included in the
#'   csv file of a NHGIS extract.
#' @param cb_file Filepath to the codebook (either the .zip file directly downloaded
#'   from the webiste, or the path to the unzipped .txt file).
#' @param data_layer A regular expression uniquely identifying the data layer to
#'   load. Required for reading from .zip files for extracts with multiple files.
#' @examples
#' \dontrun{
#' data <- read_nhgis_codebook("nhgis0001_csv.zip")
#' }
#' @family ipums_read
#' @export
read_nhgis_codebook <- function(cb_file, data_layer = NULL) {
  if (stringr::str_sub(cb_file, -4) == ".zip") {

    cb_name <- find_files_in_zip(cb_file, "txt", data_layer)
    if (length(cb_name) == 1) cb <- readr::read_lines(unz(cb_file, cb_name)) else NULL
  } else {
    cb_name <- cb_file
    if (file.exists(cb_name)) cb <- readr::read_lines(cb_name) else NULL
  }

  if (is.null(cb)) {
    stop("Could not find NHGIS codebook.")
  }

  # Get table names (var_label_long) and variable labels (var_label)
  # from data dictionary section using messy string parsing code
  dd_start <- which(
    dplyr::lag(stringr::str_detect(cb, "^[-]+$"), 2) &
      dplyr::lag(cb == "Data Dictionary", 1) &
      stringr::str_detect(cb, "^[-]+$")
  )
  dd_end <- which(
    dplyr::lead(stringr::str_detect(cb, "^[-]+$"), 1) &
      dplyr::lead(cb == "Citation and Use of NHGIS Data", 2) &
      dplyr::lead(stringr::str_detect(cb, "^[-]+$"), 3)
  )
  dd <- cb[seq(dd_start, dd_end)]

  context_rows <- seq(
    which(dd == "Context Fields ") + 1,
    which(stringr::str_detect(dd, "^Table 1:")) - 1
  )
  context_vars <- dd[context_rows]
  context_vars <- stringr::str_match(context_vars, "([:alnum:]+):[:blank:]+(.+)$")
  context_vars <- tibble::data_frame(
    var_name = context_vars[, 2],
    var_label = context_vars[, 3],
    var_label_long = ""
  )
  context_vars <- context_vars[!is.na(context_vars$var_name), ]

  table_name_rows <- which(stringr::str_detect(dd, "^Table [0-9]+:"))
  table_sections <- purrr::map2(table_name_rows, c(table_name_rows[-1], length(dd)), ~seq(.x, .y - 1))

  table_vars <- purrr::map_df(table_sections, function(rows) {
    table_name <- stringr::str_match(dd[rows[1]], "^Table [0-9]+:[:blank:]+(.+)$")[, 2]
    nhgis_table_code <- stringr::str_match(dd[rows[4]], "^NHGIS code:[:blank:]+(.+)$")[, 2]
    vars <- stringr::str_match(dd[rows[-1:-4]], "([:alnum:]+):[:blank:]+(.+)$")
    vars <- vars[!is.na(vars[, 2]), ]
    dplyr::data_frame(
      var_name = vars[, 2],
      var_label = vars[, 3],
      var_label_long = paste0(table_name, " (", nhgis_table_code, ")")
    )
  })

  # License and conditions is after data dictionary section
  conditions_text <- paste(cb[seq(dd_end + 4, length(cb))], collapse = "\n")


  out <- list(
    file_name = cb_name,
    file_path = "",
    file_type = "rectangular",
    rec_types = NULL,
    rectype_idvar = NULL,
    var_info = dplyr::bind_rows(context_vars, table_vars),
    conditions = conditions_text,
    license = NULL
  )
}
