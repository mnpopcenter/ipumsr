# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

#' Read data from an IPUMS Terra raster extract
#'
#' Read a single raster datasets downloaded from the IPUMS Terra extract system using
#' \code{read_terra_raster}, or read multiple into a list using \code{read_terra_raster_list}.
#'
#' @return
#'   For \code{read_terra_raster} A \code{\link[raster]{raster}} object, for
#'   \code{read_terra_raster_list} A list of raster objects.
#' @param data_file Filepath to the data (either the .zip file directly downloaded
#'   from the website, or the path to the unzipped .tiff file(s)).
#' @param data_layer For .zip extracts with multiple raster datasets, the name of the
#'   data to load. Accepts a character vector specifying the file name, or
#'  \code{\link{dplyr_select_style}} conventions.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @export
#' @examples
#' \dontrun{
#' data <- read_terra_raster("2552_bundle.zip", "LCDECIDOPZM2013.tiff")
#' data <- read_terra_raster_list("2552_bundle.zip", "ZM")
#' }
#' @family ipums_read
read_terra_raster <- function(
  data_file,
  data_layer = NULL,
  verbose = TRUE
) {
  data_layer <- enquo(data_layer)
  read_terra_raster_internal(data_file, data_layer, verbose, FALSE)
}

#' @rdname read_terra_raster
#' @export
read_terra_raster_list <- function(
  data_file,
  data_layer = NULL,
  verbose = TRUE
) {
  data_layer <- enquo(data_layer)
  read_terra_raster_internal(data_file, data_layer, verbose, TRUE)
}

# NB: I have these 2 functions because I want people to be able to load rasters
# without having to know about lists, but I also don't want to have the type returned
# by a function change based on the inputs. Therefore, the singular version loads directly
# as raster, and the multi one always returns a list.
read_terra_raster_internal <- function(data_file, data_layer, verbose, multiple_ok) {
  # Read data files ----
    if (path_is_zip_or_dir(data_file)) {
      tiff_names <- find_files_in(data_file, "tiff", data_layer, multiple_ok = multiple_ok)

      if (file_is_zip(data_file)) {
        raster_temp <- tempfile()
        dir.create(raster_temp)
        # Don't delete raster temp files, because R reads from disk
        utils::unzip(data_file, tiff_names, exdir = raster_temp)

        raster_paths <- file.path(raster_temp, tiff_names)
      } else {
        raster_paths <- file.path(data_file, tiff_names)
      }

      if (!multiple_ok) {
        out <- raster::raster(raster_paths)
      } else {
        out <- purrr::map(raster_paths, ~raster::raster(.))
        out <- purrr::set_names(out, fostr_sub(basename(raster_paths), 1, -6))
      }
    } else {
      if (!multiple_ok) {
        out <- raster::raster(data_file)
      } else {
        out <- purrr::map(data_file, raster::raster)
        out <- purrr::set_names(out, fostr_sub(data_file, 1, -6))
      }
    }

    if (verbose) custom_cat(short_conditions_text(terra_empty_ddi))

    out
  }

#' Read data from an IPUMS Terra area extract
#'
#' Reads a area-level dataset downloaded from the IPUMS Terra extract system.
#'
#' @return
#'   \code{read_terra_area} returns a \code{tbl_df} with the tabular data,
#'   \code{read_terra_area_sf} returns a \code{sf} object with tabular data and shapes,
#'   and \code{read_terra_area_sp} returns a \code{SpatialPolygonsDataFrame} with
#'   data and shapes.
#' @param data_file Path to the data file, which can either be the .zip file directly
#'   downloaded from the IPUMS Terra website, path to the unzipped folder, or to the
#'   csv unzipped from the download.
#' @param shape_file (Optional) If the download is unzipped, path to the .zip,
#'   folder path or .shp file representing the the shape file. If only the data
#'   table is needed, can be set to FALSE to indicate not to load the shape
#'   file.
#' @param data_layer For .zip extracts with multiple datasets, the name of the
#'   data to load. Accepts a character vector specifying the file name, or
#'  \code{\link{dplyr_select_style}} conventions. Data layer must uniquely identify
#'  a dataset.
#' @param shape_layer (Defaults to using the same value as data_layer) Specification
#'   of which shape files to load using the same semantics as \code{data_layer}. Can
#'   load multiple shape files, which will be combined.
#' @param shape_encoding The text encoding to use when reading the shape file. Typically
#'   the defaults should read the data correctly, but for some extracts you may need
#'   to set them manually, but if funny characters appear in your data, you may need
#'   to. Defaults to "UTF-8" for IPUMS Terra.
#' @param ddi_file (Optional) If the download is unzipped, path to the .xml file which
#'   provides usage and citation information for extract.
#' @param cb_file (Optional) If the download is unzipped, path to the .txt file which
#'   provides usage and citation information for extract.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @param var_attrs Variable attributes to add from the DDI, defaults to
#'   adding all (val_labels, var_label and var_desc). See
#'   \code{\link{set_ipums_var_attributes}} for more details.
#' @export
#' @examples
#' \dontrun{
#' data <- read_terra_area("2553_bundle.zip")
#' }
#' @family ipums_read
read_terra_area <- function(
  data_file,
  data_layer = NULL,
  ddi_file = NULL,
  cb_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  data_layer <- enquo(data_layer)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  data_is_zip_or_path <- path_is_zip_or_dir(data_file)

  # Try to read DDI for license info ----
  if (data_is_zip_or_path & is.null(ddi_file)) {
    ddi <- read_ipums_ddi(data_file) # Don't pass in `data_layer` bc only 1 ddi/area extract
  } else if (!is.null(ddi_file)) {
    ddi <- read_ipums_ddi(ddi_file)
  }

  # Try to read codebook for var info ----
  if (data_is_zip_or_path & is.null(cb_file)) {
    cb <- read_ipums_codebook(data_file, !!data_layer)
  } else if (!is.null(cb_file)) {
    cb <- read_ipums_codebook(cb_file)
  }

  # If both were loaded, then the ddi has better citation info, but the
  # codebook has better variable information
  if (!is.null(ddi) & !is.null(cb)) {
    ddi$var_info <- cb$var_info
    ddi$ipums_project <- cb$ipums_project
  } else if (is.null(ddi) & !is.null(cb)) {
    ddi <- cb
  } else if (is.null(ddi)) {
    ddi <- terra_empty_ddi
  }

  # Regardless of what DDI says, it appears that files are stored as UTF-8
  ddi$file_encoding <- "UTF-8"

  if (verbose) custom_cat(short_conditions_text(ddi))

  # Read data file ----
  if (file_is_zip(data_file)) {
    csv_name <- find_files_in(data_file, "csv", data_layer)
    read_data <- unz(data_file, csv_name)
  } else if (file_is_dir(data_file)) {
    csv_name <- find_files_in(data_file, "csv", data_layer)
    read_data <- file.path(data_file, csv_name)
  } else {
    read_data <- data_file
  }
  data <- readr::read_csv(
    read_data,
    col_types = readr::cols(.default = "c"),
    locale = ipums_locale(ddi$file_encoding),
    progress = show_readr_progress(verbose)
  )
  data <- readr::type_convert(data, col_types = readr::cols())

  # Add var labels and value labels from DDI, if available
  if (!is.null(ddi$var_info)) {
    data <- set_ipums_var_attributes(data, ddi$var_info, var_attrs)
  }

  data
}

#' @rdname read_terra_area
#' @export
read_terra_area_sf <- function(
  data_file,
  shape_file = NULL,
  data_layer = NULL,
  shape_layer = data_layer,
  shape_encoding = "UTF-8",
  ddi_file = NULL,
  cb_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  data_layer <- enquo(data_layer)
  data <- read_terra_area(data_file, !!data_layer, ddi_file, cb_file, verbose, var_attrs)

  shape_layer <- enquo(shape_layer)
  if (quo_text(shape_layer) == "data_layer") shape_layer <- data_layer

  if (is.null(shape_file)) shape_file <- data_file
  shape_data <- read_ipums_sf(
    shape_file, !!shape_layer, encoding = shape_encoding, verbose = verbose
  )

  geo_vars <- unname(tidyselect::vars_select(names(data), starts_with("GEO")))
  label_name <- unname(tidyselect::vars_select(geo_vars, ends_with("LABEL")))
  id_name <- dplyr::setdiff(geo_vars, label_name)[1]
  by_vars <- rlang::set_names("GEOID", id_name)

  # Shape data's label column is not reliable. Sometimes it is cut short
  # for length, etc. Rely only on the code because it is easier.
  shape_data$LABEL <- NULL

  ipums_shape_inner_join(data, shape_data, by = by_vars)
}

#' @rdname read_terra_area
#' @export
read_terra_area_sp <- function(
  data_file,
  shape_file = NULL,
  data_layer = NULL,
  shape_layer = data_layer,
  shape_encoding = "UTF-8",
  ddi_file = NULL,
  cb_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  data_layer <- enquo(data_layer)
  data <- read_terra_area(data_file, !!data_layer, ddi_file, cb_file, verbose, var_attrs)

  shape_layer <- enquo(shape_layer)
  if (quo_text(shape_layer) == "data_layer") shape_layer <- data_layer

  if (is.null(shape_file)) shape_file <- data_file
  shape_data <- read_ipums_sp(shape_file, !!shape_layer, verbose = verbose)

  # Shape data's label column is not reliable. Sometimes it is cut short
  # for length, etc. Rely only on the code because it is easier.
  shape_data$LABEL <- NULL

  geo_vars <- unname(tidyselect::vars_select(names(data), starts_with("GEO")))
  label_name <- unname(tidyselect::vars_select(geo_vars, ends_with("LABEL")))
  id_name <- dplyr::setdiff(geo_vars, label_name)[1]
  by_vars <- rlang::set_names("GEOID", id_name)

  ipums_shape_inner_join(data, shape_data, by = by_vars)
}

#' Read data from an IPUMS Terra microdata extract
#'
#' Reads a microdata dataset downloaded from the IPUMS Terra extract system.
#'
#' @return
#'   \code{read_terra_micro} returns a \code{tbl_df} with the tabular data. Use
#'   \code{\link{read_ipums_sf}} or \code{\link{read_ipums_sp}} to read shape
#'   data out of a microdata Terra extract.
#' @param data_file Path to the data file, which can either be the .zip file directly
#'   downloaded from the IPUMS Terra website, a path to the unzipped version of that
#'   folder, or to the csv unzipped from the download.
#' @param ddi_file (Optional) If the download is unzipped, path to the .xml file which
#'   provides usage and citation information for extract.
#' @param data_layer For .zip extracts with multiple datasets, the name of the
#'   data to load. Accepts a character vector specifying the file name, or
#'  \code{\link{dplyr_select_style}} conventions. Data layer must uniquely identify
#'  a dataset.
#' @param n_max Maximum number of observations to read from the data
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @param var_attrs Variable attributes to add from the DDI, defaults to
#'   adding all (val_labels, var_label and var_desc). See
#'   \code{\link{set_ipums_var_attributes}} for more details.
#' @export
#' @examples
#' \dontrun{
#' data <- read_terra_micro("2553_bundle.zip")
#' }
#' @family ipums_read
read_terra_micro <- function(
  data_file,
  ddi_file = NULL,
  data_layer = NULL,
  n_max = Inf,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  data_layer <- enquo(data_layer)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  data_is_zip_or_path <- path_is_zip_or_dir(data_file)

  # Try to read DDI for license info ----
  if (data_is_zip_or_path & is.null(ddi_file)) {
    ddi <- read_ipums_ddi(data_file)
  } else if (!is.null(ddi_file)) {
    ddi <- read_ipums_ddi(ddi_file)
  } else {
    ddi <- terra_empty_ddi
  }
  if (is.na(ddi$ipums_project)) ddi$ipums_project <- "IPUMS Terra"

  if (verbose) custom_cat(short_conditions_text(ddi))

  # Read data file ----
  if (file_is_zip(data_file)) {
    csv_name <- find_files_in(data_file, "(csv|gz)", data_layer)
    read_data <- unz(data_file, csv_name)
    if (fostr_sub(csv_name, -3) == ".gz") {
      read_data <- gzcon(read_data)
    }
  } else if (file_is_dir(data_file)) {
    csv_name <- find_files_in(data_file, "(csv|gz)", data_layer)
    read_data <- file.path(data_file, csv_name)
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
      switch(
        x,
        "numeric" = readr::col_number(),
        "character" = readr::col_character(),
        "integer" = readr::col_integer()
      )
    })
    var_type_spec <- purrr::set_names(var_type_spec, var_type_info$var_name)
    var_types <- readr::cols(.defualt = readr::col_character())
    var_types$cols <- var_type_spec
  }

  data <- readr::read_csv(
    read_data,
    col_types = var_types,
    n_max = n_max,
    na = "null",
    locale = ipums_locale(ddi$file_encoding),
    progress = show_readr_progress(verbose)
  )

  if (any(purrr::map_lgl(data, is.character))) {
    data <- readr::type_convert(data, readr::cols())
  }


  # Add var labels and value labels from DDI, if available
  if (!is.null(ddi$var_info)) {
    all_vars <- ddi$var_info
    data <- set_ipums_var_attributes(data, ddi$var_info, var_attrs)
  }

  data
}


# Fills in a default condition if we can't find ddi for terra
terra_empty_ddi <- make_ddi_from_scratch(
  ipums_project = "IPUMS Terra",
  file_type = "rectangular",
  conditions = paste0(
    "Use of IPUMS Terra data is subject to conditions, including that ",
    "publications and research which employ IPUMS Terra data should cite it ",
    "appropiately. Please see www.terrapop.org for more information."
  )
)
