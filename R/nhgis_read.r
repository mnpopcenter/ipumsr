#' Read data from an NHGIS extract
#'
#' Reads a dataset downloaded from the NHGIS extract system. Depends on
#' the extract table file structure "Comma delimited with header" and the
#' "Include additional descriptive header row" options selected.
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
#' data <- ip_read_nhgis("nhgis0001_csv.zip", "nhgis0001_shp.zip")
#' }
#' @seealso ip_read_data ip_read_terra_raster ip_read_terra_area
#' @export
ip_read_nhgis <- function(
  data_file,
  shape_file = NULL,
  data_layer = NULL,
  shape_layer = NULL,
  verbose = TRUE
) {
  # Read data files ----
  data_is_zip <- stringr::str_sub(data_file, -4) == ".zip"
  if (data_is_zip) {
    data_file_names <- utils::unzip(data_file, list = TRUE)$Name
    # Find data
    csv_name <- stringr::str_subset(data_file_names, "\\.csv$")

    if (!is.null(data_layer)) csv_name <- stringr::str_subset(csv_name, data_layer)

    if (length(csv_name) > 1) {
      stop(paste0(
        "Multiple data files found, please use the `data_layer` argument to ",
        " specify which layer you want to load.\n", paste(csv_name, collapse = ", ")
        ), .call = FALSE)
    }

    # Find codebook
    cb_name <- stringr::str_subset(data_file_names, "codebook\\.txt$")
    if (!is.null(data_layer)) cb_name <- stringr::str_subset(cb_name, data_layer)
    cb_exists <- length(cb_name) == 1
  } else {
    cb_name <- stringr::str_replace(data_file, "\\.txt$", "_codebook\\.txt")
    cb_exists <- file.exists(cb_name)
  }

  # Print license info (if available)
  if (cb_exists) {
    if (data_is_zip) cb_name <- unz(data_file, cb_name)
    cb <- readr::read_lines(cb_name)

    license_start <- which(cb == "Citation and Use of NHGIS Data") - 1
    if (verbose) cat(paste(cb[seq(license_start, length(cb))], collapse = "\n"))
  } else {
    if (verbose) cat(paste0(
      "Use of NHGIS data is subject to conditions, including that ",
      "publications and research which employ NHGIS data should cite it",
      "appropiately. Please see www.nhgis.org for more information."
    ))
  }

  # Read data
  if (verbose) cat("\n\nReading data file...\n")
  if (data_is_zip) {
    # Could use open="rb", but then readr leaves connection open pointed to end.
    # Documentation implies seeking in Windows is untrustworthy, so I want to
    # reopen it each time anyways.
    read_data <- unz(data_file, csv_name)
  } else {
    read_data <- data_file
  }

  var_info <- readr::read_csv(
    read_data,
    n_max = 1,
    col_types = readr::cols(.default = "c")
  )
  if (data_is_zip) read_data <- unz(data_file, csv_name) # Reopen connection
  data <- readr::read_csv(
    read_data,
    skip = 2,
    col_names = names(var_info),
    col_types = readr::cols(.default = "c")
  )
  data <- readr::type_convert(col_types, col_types = readr::cols())

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
            " specify which layer you want to load.\n", paste(shp_zips, collapse = ", ")
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
            stringr::str_sub(shape_shps, -4),
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
  purrr::walk2(names(var_info), unname(unlist(var_info)), function(vname, vlabel, ...) {
    attr(data[[vname]], "label") <<- vlabel
  })

  data
}
