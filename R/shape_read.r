# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport


#' Read boundary files from an IPUMS extract
#'
#' Reads the boundary files form an IPUMS extract into R.
#'
#' @param shape_file Filepath to one or more .shp files or a .zip file from an IPUMS extract
#' @param shape_layer For .zip extracts with multiple datasets, the name of the
#'   shape files to load. Accepts a character vector specifying the file name, or
#'  \code{\link{dplyr_select_style}} conventions. Can load multiple shape files,
#'    which will be combined.
#' @param bind_multiple If \code{TRUE}, will combine multiple shape files found into
#'   a single object.
#' @examples
#' \dontrun{
#' boundaries <- read_ipums_sf("nhgis_00001.zip")
#' }
#' @family ipums_read
#' @export
read_ipums_sf <- function(shape_file, shape_layer = NULL, bind_multiple = TRUE) {
  shape_layer <- enquo(shape_layer)
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
      shape_temp <- tempfile()
      dir.create(shape_temp)
      on.exit(unlink(shape_temp, recursive = TRUE))

       purrr::walk(shape_zips, function(x) {
        utils::unzip(shape_file, x, exdir = shape_temp)
        utils::unzip(file.path(shape_temp, x), exdir = shape_temp)
      })
       read_shape_files <- dir(shape_temp, "\\.shp$", full.names = TRUE)
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
        shape_temp <- tempfile()
        dir.create(shape_temp)
        on.exit(unlink(shape_temp, recursive = TRUE))

        read_shape_files <- purrr::map_chr(shape_shps, function(x) {
          shape_shp_files <- paste0(
            stringr::str_sub(x, 1, -4),
            c("shp", "dbf", "prj", "sbn", "sbx", "shx")
          )

          utils::unzip(shape_file, shape_shp_files, exdir = shape_temp)

          file.path(shape_temp, shape_shp_files[1])
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

  out <- purrr::map(read_shape_files, ~sf::read_sf(., options = "ENCODING=UTF-8"))
  out <- careful_sf_rbind(out)

  out
}

# Takes a list of sf's, fills in empty columns for you and binds them together.
# Throws error if types don't match
careful_sf_rbind <- function(sf_list) {
  if (length(sf_list) == 1) {
    return(sf_list[[1]])
  } else {
    # Get var info for all columns
    all_var_info <- purrr::map_df(sf_list, .id = "id", function(x) {
      tibble::data_frame(name = names(x), type = purrr::map(x, ~class(.)))
    })

    var_type_check <- dplyr::group_by(all_var_info, .data$name)
    var_type_check <- dplyr::summarize(var_type_check, check = length(unique(.data$type)))
    if (any(var_type_check$check != 1)) {
      stop("Cannot combine shape files because variable types don't match.")
    }

    all_var_info$id <- NULL
    all_var_info <- dplyr::distinct(all_var_info)

    out <- purrr::map(sf_list, function(x) {
      missing_vars <- dplyr::setdiff(all_var_info$name, names(x))
      if (length(missing_vars) == 0) return(x)

      for (vn in missing_vars) {
        vtype <- all_var_info$type[all_var_info$name == vn][[1]]
        if (identical(vtype, "character")) x[[vn]] <- NA_character_
        else if (identical(vtype, "numeric")) x[[vn]] <- NA_real_
        else if (identical(vtype, c("sfc_MULTIPOLYGON", "sfc"))) x[[vn]] <- vector("list", nrow(x))
        else stop("Unexpected variable type in shape file.")
      }
      x
    })
    out <- do.call(rbind, out)
  }
  sf::st_as_sf(tibble::as.tibble(out))
}
