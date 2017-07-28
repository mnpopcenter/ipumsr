#' List files available for analysis in an IPUMS extract zip file
#'
#' Find which files can be loaded from an IPUMS extract zip file.
#'
#' @param file An IPUMS extract zip file
#' @param types One or more of "data", "shape", or "raster" indicating
#'   what type of files to look for.
#' @param data_layer A regex filter for data files to look for
#' @param shape_layer A regex filter for shape files to look for
#' @param raster_layer A regex filter for raster files to look for
#' @export
ip_list_files <- function(file, types = NULL, data_layer = NULL,
                          shape_layer = NULL, raster_layer = NULL) {
  if (!file_is_zip(file)) stop("File must be a .zip file")

  if (is.null(types) | "data" %in% types) {
    data_files <- ip_list_data_files(file, data_layer)
  }

  if (is.null(types) | "shape" %in% types) {
    shape_files <- ip_list_shape_files(file, shape_layer)
  }

  if (is.null(types) | "raster" %in% types) {
    raster_files <- ip_list_raster_files(file, raster_layer)
  }

  dplyr::bind_rows(data = data_files, shape = shape_files, raster = raster_files, .id = "type")
}

#' @rdname ip_list_files
#' @export
ip_list_data_files <- function(file, data_layer = NULL) {
  if (!file_is_zip(file)) stop("File must be a .zip file")
  tibble::data_frame(
    file = find_files_in_zip(file, "(dat|csv)(\\.gz)?", data_layer, TRUE)
  )
}

#' @rdname ip_list_files
#' @export
ip_list_shape_files <- function(file, shape_layer = NULL) {
  if (!file_is_zip(file)) stop("File must be a .zip file")
  tibble::data_frame(
    file = find_files_in_zip(file, "(zip|shp)", shape_layer, TRUE)
  )
}

#' @rdname ip_list_files
#' @export
ip_list_raster_files <- function(file, raster_layer = NULL) {
  if (!file_is_zip(file)) stop("File must be a .zip file")
  tibble::data_frame(
    file = find_files_in_zip(file, "tiff", raster_layer, TRUE)
  )
}


