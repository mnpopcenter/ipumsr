#' Read data from an IPUMS Terra raster extract
#'
#' Reads a raster dataset downloaded from the IPUMS Terra extract system.
#'
#' @return
#'   A \code{\link[raster]{raster}} object
#' @param data_file Filepath to the data (either the .zip file directly downloaded
#'   from the webiste, or the path to the unzipped .tiff file).
#' @param data_layer A regular expression uniquely identifying the data layer to
#'   load. Required for reading from .zip files for extracts with multiple files.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @examples
#' \dontrun{
#' data <- ip_read_terra_raster("2552_bundle.zip", "LCDECIDOPZM2013.tiff")
#' }
#' @seealso ip_read_terra_area ip_read_data ip_read_nhgis
#' @export
ip_read_terra_raster <- function(
  data_file,
  data_layer = NULL,
  verbose = TRUE
) {
  # Read data files ----
  data_is_zip <- stringr::str_sub(data_file, -4) == ".zip"
  if (data_is_zip) {
    data_file_names <- utils::unzip(data_file, list = TRUE)$Name
    # Find data
    tiff_name <- stringr::str_subset(data_file_names, "\\.tiff$")

    if (!is.null(data_layer)) tiff_name <- stringr::str_subset(tiff_name, data_layer)

    if (length(tiff_name) > 1) {
      stop(paste0(
        "Multiple data files found, please use the `data_layer` argument to ",
        " specify which layer you want to load.\n", paste(tiff_name, collapse = ", ")
      ), .call = FALSE)
    }

    raster_temp <- tempfile()
    utils::unzip(data_file, tiff_name, exdir = raster_temp)

    out <- raster::raster(raster_temp)
  } else {
    out <- raster::raster(data_file)
  }

  # Print license info (not provided in extract for area-level terra)
  if (verbose) cat(paste0(
    "Use of IPUMS Terra data is subject to conditions, including that ",
    "publications and research which employ IPUMS Terra data should cite it",
    "appropiately. Please see www.terrapop.org for more information."
  ))

  out
}

#' Read data from an IPUMS Terra area extract
#'
#' Reads a area-level dataset downloaded from the IPUMS Terra extract system.
#'
#' @return
#'   A \code{\link[raster]{raster}} object
#' @param data_file Path to the data file, which can either be the .zip file directly
#'   downloaded from the IPUMS Terra website, or to the csv unzipped from the download.
#' @param ddi_file (Optional) If the download is unzipped, path to the .xml file which
#'   provides usage and ciation information for extract.
#' @param shape_file (Optional) If the download is unzipped, path to the .zip or .shp file
#'   representing the the shape file. If only the data table is needed, can be set to FALSE
#'   to indicate not to load the shape file.
#' @param data_layer If data_file is an extract with multiple csvs, a regular expression indicating
#'   which .csv data layer to load.
#' @param shape_layer If data_file is an extract with multiple shape files, a regular expression
#'   indicating which shape layer to load.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @examples
#' \dontrun{
#' data <- ip_read_terra_area("2553_bundle.zip")
#' }
#' @seealso ip_read_terra_raster ip_read_data ip_read_nhgis
#' @export
ip_read_terra_area <- function(
  data_file,
  ddi_file = NULL,
  shape_file = NULL,
  data_layer = NULL,
  shape_layer = NULL,
  verbose = TRUE
) {
  # Extract file names if passed a zip file
  data_is_zip <- stringr::str_sub(data_file, -4) == ".zip"
  if (data_is_zip) {
    data_file_names <- utils::unzip(data_file, list = TRUE)$Name
  }

  # Try to read DDI for license info ----
  if (data_is_zip & is.null(ddi_file)) {
    ddi_file_in_zip <- stringr::str_subset(data_file_names, "\\.xml$")
    if (length(ddi_file_in_zip > 0)) {
      ddi_file_in_zip <- ddi_file_in_zip[1]
      ddi_file <- file.path(tempdir(), ddi_file_in_zip)
      utils::unzip(data_file, ddi_file_in_zip, exdir = tempdir())
    }
  }

  if (!is.null(ddi_file)) {
    ddi <- ip_read_ddi(ddi_file)
  } else {
    ddi <- NULL
  }

  # Print the license info
  if (verbose) {
    if (!is.null(ddi)) {
      cat(ddi$conditions)
      cat("\n\n")
      cat(ddi$citation)
      cat("\n\n")
    } else {
      cat(paste0(
        "Use of IPUMS Terra data is subject to conditions, including that ",
        "publications and research which employ IPUMS Terra data should cite it",
        "appropiately. Please see www.terrapop.org for more information."
      ))
    }
  }

  # Read data file ----
  if (data_is_zip) {
    csv_name <- stringr::str_subset(data_file_names, "\\.csv$")
    if (!is.null(data_layer)) csv_name <- stringr::str_subset(csv_name, data_layer)

    if (length(csv_name) > 1) {
      stop(paste0(
        "Multiple data files found, please use the `data_layer` argument to ",
        " specify which layer you want to load.\n", paste(csv_name, collapse = ", ")
      ), .call = FALSE)
    }
    read_data <- unz(data_file, csv_name)
  } else {
    read_data <- data_file
  }
  data <- readr::read_csv(read_data, col_types = readr::cols(.default = "c"))
  data <- readr::type_convert(data, col_types = readr::cols())

  # Try to read shape file ----
  # Don't bother looking for shape file if not specified or if
  # explicitly told not to
  if (rlang::is_false(shape_file) | (!data_is_zip & is.null(shape_file))) {
    out <- data
  } else {
    if (data_is_zip) {
      # Look for zipped shape file within full data zip
      zip_name <- stringr::str_subset(data_file_names, "\\.zip$")
      if (!is.null(shape_layer)) zip_name <- stringr::str_subset(zip_name, data_layer)

      if (length(zip_name) > 1) {
        stop(paste0(
          "Multiple shape files found, please use the `shape_layer` argument to ",
          " specify which layer you want to load.\n", paste(zip_name, collapse = ", ")
        ), .call = FALSE)
      }
      shape_temp <- tempfile()
      dir.create(shape_temp)
      utils::unzip(data_file, zip_name, exdir = shape_temp)
      utils::unzip(file.path(shape_temp, zip_name), exdir = shape_temp)
      read_shape_file <- dir(shape_temp, pattern = ".shp$", full.names = TRUE)
    } else {
      shape_is_zip <- stringr::str_sub(shape_file, -4) == ".zip"
      if (shape_is_zip) {
        shape_file_names <- utils::unzip(data_file, list = TRUE)$Name
        shp_name <- stringr::str_subset(shape_file_names, "\\.shp$")
        if (!is.null(shape_layer)) shp_name <- stringr::str_subset(shp_name, data_layer)

        if (length(shp_name) > 1) {
          stop(paste0(
            "Multiple shape files found, please use the `shape_layer` argument to ",
            " specify which layer you want to load.\n", paste(shape_name, collapse = ", ")
          ), .call = FALSE)
        }
        shape_shp_files <- paste0(
          stringr::str_sub(shp_name, 1, -4),
          c("dbf", "prj", "sbn", "sbx", "shp", "shp.xml", "shx")
        )
        shape_temp <- tempfile()
        dir.create(shape_temp)
        utils::unzip(shape_file, shape_shp_files, exdir = shape_temp)

        read_shape_file <- file.path(shape_temp, shape_shps)
      } else {
        read_shape_file <- shape_file
      }
    }

    shape_data <- sf::read_sf(read_shape_file, options = "ENCODING=UTF-8")

    # TODO: This join seems like it is fragile. Is there a better way?
    geo_vars <- unname(dplyr::select_vars(names(data), starts_with("GEO")))
    label_name <- unname(dplyr::select_vars(geo_vars, ends_with("LABEL")))
    id_name <- dplyr::setdiff(geo_vars, label_name)[1]

    out <- dplyr::full_join(shape_data, data, by = c(LABEL = label_name, GEOID = id_name))
    out <- sf::st_as_sf(tibble::as_tibble(out))
  }

  out
}

#' Read data from an IPUMS Terra microdata extract
#'
#' Reads a microdata dataset downloaded from the IPUMS Terra extract system.
#'
#' @return
#'   If a shape file is found, a list containing a data.frame with the microdata,
#'   and a sf with the geography. Otherwise, just a data.frame with the microdata.
#' @param data_file Path to the data file, which can either be the .zip file directly
#'   downloaded from the IPUMS Terra website, or to the csv unzipped from the download.
#' @param ddi_file (Optional) If the download is unzipped, path to the .xml file which
#'   provides usage and ciation information for extract.
#' @param shape_file (Optional) If the download is unzipped, path to the .zip or .shp file
#'   representing the the shape file. If only the data table is needed, can be set to FALSE
#'   to indicate not to load the shape file.
#' @param data_layer If data_file is an extract with multiple csvs, a regular expression indicating
#'   which .csv data layer to load.
#' @param shape_layer If data_file is an extract with multiple shape files, a regular expression
#'   indicating which shape layer to load.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @examples
#' \dontrun{
#' data <- ip_read_terra_micro("2553_bundle.zip")
#' }
#' @seealso ip_read_terra_raster ip_read_data ip_read_nhgis
#' @export
ip_read_terra_micro <- function(
  data_file,
  ddi_file = NULL,
  shape_file = NULL,
  data_layer = NULL,
  shape_layer = NULL,
  verbose = TRUE
) {
  # Extract file names if passed a zip file
  data_is_zip <- stringr::str_sub(data_file, -4) == ".zip"
  if (data_is_zip) {
    data_file_names <- utils::unzip(data_file, list = TRUE)$Name
  }

  # Try to read DDI for license info ----
  if (data_is_zip & is.null(ddi_file)) {
    ddi_file_in_zip <- stringr::str_subset(data_file_names, "\\.xml$")
    if (length(ddi_file_in_zip > 0)) {
      ddi_file_in_zip <- ddi_file_in_zip[1]
      ddi_file <- file.path(tempdir(), ddi_file_in_zip)
      utils::unzip(data_file, ddi_file_in_zip, exdir = tempdir())
    }
  }

  if (!is.null(ddi_file)) {
    ddi <- ip_read_ddi(ddi_file)
  } else {
    ddi <- NULL
  }

  # Print the license info
  if (verbose) {
    if (!is.null(ddi)) {
      cat(ddi$conditions)
      cat("\n\n")
      cat(ddi$citation)
      cat("\n\n")
    } else {
      cat(paste0(
        "Use of IPUMS Terra data is subject to conditions, including that ",
        "publications and research which employ IPUMS Terra data should cite it",
        "appropiately. Please see www.terrapop.org for more information."
      ))
    }
  }

  # Read data file ----
  if (data_is_zip) {
    csv_name <- stringr::str_subset(data_file_names, "\\.csv(\\.gz)?$")
    if (!is.null(data_layer)) csv_name <- stringr::str_subset(csv_name, data_layer)

    if (length(csv_name) > 1) {
      stop(paste0(
        "Multiple data files found, please use the `data_layer` argument to ",
        " specify which layer you want to load.\n", paste(csv_name, collapse = ", ")
      ), .call = FALSE)
    }
    read_data <- unz(data_file, csv_name)
    if (stringr::str_sub(csv_name, -3) == ".gz") {
      read_data <- gzcon(read_data)
    }
  } else {
    read_data <- data_file
  }

  # Use DDI for vartype information, if available
  if (!is.null(ddi$var_info$var_type)) {
    # IPUMS Terra microdata DDIs are messy. They have a non-exhaustive list of variables with
    # multiple versions for some of them.
    # Get unique list of col_types provided, and read the rest in as characters.
    # Then guess based on all values using `readr::type_convert()`
    var_type_info <- ddi$var_info
    var_type_info <- var_type_info[, c("var_name", "var_type")]
    var_type_info <- dplyr::distinct(var_type_info)
    var_type_spec <- purrr::map(var_type_info$var_type, function(x) {
      switch(x, "numeric" = readr::col_number(), "character" = readr::col_character())
    })
    var_type_spec <- purrr::set_names(var_type_spec, var_type_info$var_name)
    var_types <- readr::cols(.defualt = readr::col_character())
    var_types$cols <- var_type_spec

    data <- readr::read_csv(read_data, col_types = var_types, na = "null")

    data <- readr::type_convert(data, readr::cols())
  } else {
    data <- readr::read_csv(read_data, col_types = readr::cols(.default = "c"))
    data <- readr::type_convert(data, col_types = readr::cols())
  }

  # Add var labels and value labels from DDI, if available
  if (!is.null(ddi)) {
    all_vars <- ddi$var_info
    purrr::walk2(all_vars$var_name, all_vars$val_label, function(var, lbls) {
      if (nrow(lbls) == 0) return(var)
      data[[var]] <<- haven::labelled(data[[var]], purrr::set_names(lbls$val, lbls$lbl))
    })
    purrr::walk2(
      all_vars$var_name, all_vars$var_label, function(.x, .y) {
        data[[.x]] <<- rlang::set_attrs(data[[.x]], label = .y)
    })
    purrr::walk2(
      all_vars$var_name, all_vars$var_label_long, function(.x, .y) {
        data[[.x]] <<- rlang::set_attrs(data[[.x]], label_long = .y)
    })
  }

  # Try to read shape file ----
  # Don't bother looking for shape file if not specified or if
  # explicitly told not to
  if (rlang::is_false(shape_file) | (!data_is_zip & is.null(shape_file))) {
    out <- data
  } else {
    if (data_is_zip) {
      # Look for zipped shape file within full data zip
      zip_name <- stringr::str_subset(data_file_names, "\\.zip$")
      if (!is.null(shape_layer)) zip_name <- stringr::str_subset(zip_name, data_layer)

      if (length(zip_name) > 1) {
        stop(paste0(
          "Multiple shape files found, please use the `shape_layer` argument to ",
          " specify which layer you want to load.\n", paste(zip_name, collapse = ", ")
        ), .call = FALSE)
      }
      shape_temp <- tempfile()
      dir.create(shape_temp)
      utils::unzip(data_file, zip_name, exdir = shape_temp)
      utils::unzip(file.path(shape_temp, zip_name), exdir = shape_temp)
      read_shape_file <- dir(shape_temp, pattern = ".shp$", full.names = TRUE)
    } else {
      shape_is_zip <- stringr::str_sub(shape_file, -4) == ".zip"
      if (shape_is_zip) {
        shape_file_names <- utils::unzip(data_file, list = TRUE)$Name
        shp_name <- stringr::str_subset(shape_file_names, "\\.shp$")
        if (!is.null(shape_layer)) shp_name <- stringr::str_subset(shp_name, data_layer)

        if (length(shp_name) > 1) {
          stop(paste0(
            "Multiple shape files found, please use the `shape_layer` argument to ",
            " specify which layer you want to load.\n", paste(shape_name, collapse = ", ")
          ), .call = FALSE)
        }
        shape_shp_files <- paste0(
          stringr::str_sub(shp_name, 1, -4),
          c("dbf", "prj", "sbn", "sbx", "shp", "shp.xml", "shx")
        )
        shape_temp <- tempfile()
        dir.create(shape_temp)
        utils::unzip(shape_file, shape_shp_files, exdir = shape_temp)

        read_shape_file <- file.path(shape_temp, shape_shps)
      } else {
        read_shape_file <- shape_file
      }
    }

    shape_data <- sf::read_sf(read_shape_file, options = "ENCODING=UTF-8")

    # TODO: We could join if we nested the data.frames for each geography
    geo_var <- unname(dplyr::select_vars(names(data), starts_with("GEO")))[1]
    shape_data[[geo_var]] <- shape_data$GEOID

    out <- list(
      data = data,
      shape = shape_data
    )
    out
  }

  out
}
