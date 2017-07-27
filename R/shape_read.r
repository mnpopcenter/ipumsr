#' Read boundary files from an IPUMS extract
#'
#' Reads the boundary files form an IPUMS extract into R.
#'
#' @param shape_file Filepath to a .shp file or .zip file from an IPUMS extract
#' @param shape_layer A regular expression selecting the layers you wish to load
#' @param bind_multiple If \code{TRUE}, will combine multiple shape files found into
#'   a single object.
#' @examples
#' \dontrun{
#' boundaries <- read_ipums_sf("nhgis_00001.zip")
#' }
#' @family ipums_read
#' @export
read_ipums_sf <- function(shape_file, shape_layer, bind_multiple = TRUE) {
  load_sf_namespace()

  # Case 1: Shape file specified is a .zip file
  shape_is_zip <- stringr::str_sub(shape_file, -4) == ".zip"
  if (shape_is_zip) {
    read_shape_files <- character(0) # Start with empty list of files to read
    # Case 1a: First zip file has zip files of shape files within it
    shape_zips <- find_files_in_zip(shape_file, "zip", shape_layer, multiple_ok = TRUE)

    if (!bind_multiple && length(shape_zips) > 1) {
      stop(paste0(
        "Multiple shape files found, please set the `bind_multiple` argument to `TRUE`",
        " to combine them together, or use the `shape_layer` argument to specify a",
        " single layer.\n", paste(shape_zips, collapse = ", ")
      ))
    }

    if (length(shape_zips) >= 1) {
      read_shape_files <- purrr::map_chr(shape_zips, function(x) {
        shape_temp <- tempfile()
        dir.create(shape_temp)
        utils::unzip(shape_file, x, exdir = shape_temp)
        utils::unzip(file.path(shape_temp, shape_zips), exdir = shape_temp)

        file.path(shape_temp, dir(shape_temp, "\\.shp$"))
      })
    }

    # Case 1b: First zip file has .shp files within it
    if (length(read_shape_files) == 0) {
      shape_shps <- find_files_in_zip(shape_file, "shp", shape_layer, multiple_ok = TRUE)

      if (!bind_multiple && length(shape_shps) > 1) {
        stop(paste0(
          "Multiple shape files found, please set the `bind_multiple` argument to `TRUE`",
          " to combine them together, or use the `shape_layer` argument to specify a",
          " single layer.\n", paste(shape_shps, collapse = ", ")
        ))
      }

      if (length(shape_shps) >= 1) {
        read_shape_files <- purrr::map_chr(shape_shps, function(x) {
          shape_shp_files <- paste0(
            stringr::str_sub(x, 1, -4),
            c("dbf", "prj", "sbn", "sbx", "shp", "shx")
          )
          shape_temp <- tempfile()
          dir.create(shape_temp)
          utils::unzip(shape_file, shape_shp_files, exdir = shape_temp)

          file.path(shape_temp, dir(shape_temp, "\\.shp$"))
        })
      }

      if (length(read_shape_files) == 0) {
        stop(call. = FALSE, paste0(
          "Zip file not formatted as expected. Please check your `shape_layer`",
          "argument or unzip and try again."
        ))
      }
    }
  }

  # Case 2: Shape file specified is a .shp file
  shape_is_shp <- stringr::str_sub(shape_file, -4) == ".shp"
  if (shape_is_shp) {
    read_shape_files <- shape_file
  }

  if (!shape_is_zip & !shape_is_shp) {
    stop("Expected `shape_file` to be a .zip or .shp file.")
  }

  out <- purrr::map(shape_files, sf::read_sf)
  out <- sf::rbind(out)
  out
}
