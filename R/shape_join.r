# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums


#' Join data to geographic boundaries
#'
#' Helpers for joining shape files downloaded from the IPUMS website to data from extracts.
#' Because of historical reasons, the attributes of (like variable type) of variables
#' in the shape files does not always match those in the data files.
#'
#' @param data A dataset, usually one that has been aggregated to a geographic level.
#' @param shape_data A shape file (loaded with \code{\link{read_ipums_sf}} or \code{read_ipums_sp})
#' @param by A vector of variable names to join on. Like the dplyr join functions, named vectors
#'   indicate that the names are different between the data and shape file.
#'   shape files to load. Accepts a character vector specifying the file name, or
#'  \code{\link{dplyr_select_style}} conventions. Can load multiple shape files,
#'    which will be combined.
#' @param suffix For variables that are found in both, but aren't joined on, a suffix
#'   to put on the variables. Defaults to nothing for data variables and "_SHAPE" for
#'   variables from the shape file.
#' @param verbose I \code{TRUE}, will report information about geometries dropped in the merge.
#' @return returns a sf or a SpatialPolygonsDataFrame depending on what was passed in.
#' @examples
#' # Note that these examples use NHGIS data so that they use the example data provided,
#' # but the functions read_nhgis_sf/read_nhgis_sp perform this merge for you.
#'
#' data <- read_nhgis(ripums_example("nhgis0008_csv.zip"))
#'
#' if (require(sf)) {
#'   sf <- read_ipums_sf(ripums_example("nhgis0008_shape_small.zip"))
#'   data_sf <- ipums_shape_inner_join(data, sf, by = "GISJOIN")
#' }
#'
#' if (require(sp)) {
#'   sp <- read_ipums_sp(ripums_example("nhgis0008_shape_small.zip"))
#'   data_sp <- ipums_shape_inner_join(data, sp, by = "GISJOIN")
#' }
#'
#' \dontrun{
#'   # Sometimes variable names won't match between datasets (for example in IPUMS international)
#'   data <- read_ipums_micro("ipumsi_00004.xml")
#'   shape <- read_ipums_sf("geo2_br1980_2010.zip")
#'   data_sf <- ipums_shape_inner_join(data, shape, by = c("GEO2" = "GEOLEVEL2"))
#' }
#'
#' @export
ipums_shape_left_join <- function(data, shape_data, by, suffix = c("", "SHAPE"), verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "left", suffix, verbose)
}

#' @rdname ipums_shape_left_join
#' @export
ipums_shape_right_join <- function(data, shape_data, by, suffix = c("", "SHAPE"), verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "right", suffix, verbose)
}

#' @rdname ipums_shape_left_join
#' @export
ipums_shape_inner_join <- function(data, shape_data, by, suffix = c("", "SHAPE"), verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "inner", suffix, verbose)
}

#' @rdname ipums_shape_left_join
#' @export
ipums_shape_full_join <- function(data, shape_data, by, suffix = c("", "SHAPE"), verbose = TRUE) {
  ipums_shape_join(data, shape_data, by, "full", suffix, verbose)
}


ipums_shape_join <- function(
  data,
  shape_data,
  by,
  direction = c("full", "inner", "left", "right"),
  suffix = c("", "_SHAPE"),
  verbose = TRUE
) {
  UseMethod("ipums_shape_join", shape_data)
}

ipums_shape_join.sf <- function(
  data,
  shape_data,
  by,
  direction = c("full", "inner", "left", "right"),
  suffix = c("", "_SHAPE"),
  verbose = TRUE
) {
  if (is.null(names(by))) {
    by_shape <- by
    by_data <- by
  } else {
    # Can be a mix of named arguments and unnamed ones. If unnamed
    # use the value as name.
    names(by)[names(by) == ""] <- unname(by)[names(by) == ""]
    by_shape <- unname(by)
    by_data <- names(by)
  }
  direction <- match.arg(direction)

  check_shape_join_names(by_shape, names(shape_data), "shape data")
  check_shape_join_names(by_data, names(data), "data")

  # We're pretending like the x in the join is the data, but
  # because of the join functions dispatch, we will actually be
  # doing the reverse. Therefore, rename the variables in shape,
  # and also reverse the suffix.
  if (!is.null(names(by))) {
    shape_data <- dplyr::rename(shape_data, !!!rlang::syms(by))
    by <- names(by)
  }
  suffix <- rev(suffix)
  if (direction == "left") direction <- "right"
  if (direction == "right") direction <- "left"

  alligned <- allign_id_vars(shape_data, data, by)
  merge_f <- utils::getFromNamespace(paste0(direction, "_join"), "dplyr")
  out <- merge_f(alligned$shape_data, alligned$data, by = by, suffix = suffix)
  attr(out, "sf_column") <- attr(shape_data, "sf_column")

  # message for merge failures
  if (verbose) {
    join_fail_attributes <- check_for_join_failures(out, by, alligned$shape_data, alligned$data)
  }
  # Bring variables in data to front of data.frame (but need to get names
  # after possibly renamed by suffix)
  dvars <- names(data)
  renamed <- dvars[dvars %in% names(shape_data) & !dvars %in% by]
  if (length(renamed) > 0) {
    dvars[dvars %in% renamed] <- paste0(renamed, suffix[2])
  }
  out <- dplyr::select(out, dplyr::one_of(dvars), dplyr::everything())

  if (verbose) attr(out, "join_failures") <- join_fail_attributes
  out
}

ipums_shape_join.SpatialPolygonsDataFrame <- function(
  data,
  shape_data,
  by,
  direction = c("full", "inner", "left", "right"),
  suffix = c("", "_SHAPE"),
  verbose = TRUE
) {
  if (is.null(names(by))) {
    by_shape <- by
    by_data <- by
  } else {
    # Can be a mix of named arguments and unnamed ones. If unnamed
    # use the value as name.
    names(by)[names(by) == ""] <- unname(by)[names(by) == ""]
    by_shape <- unname(by)
    by_data <- names(by)
  }
  direction <- match.arg(direction)
  if (direction %in% c("left", "full")) { # Note that directions are reversed because of dispatch
    stop(paste0(
      "Only inner and right joins are supported for SpatialPolygonsDataFrame (sp) data ",
      "because non-matched observations from the data would create NULL geometries which ",
      "are not allowed in the sp package."
    ))
  }

  check_shape_join_names(by_shape, names(shape_data@data), "shape data")
  check_shape_join_names(by_data, names(data), "data")

  # We're pretending like the x in the join is the data, but
  # because of the join functions dispatch, we will actually be
  # doing the reverse. Therefore, rename the variables in shape,
  # and also reverse the suffix.
  if (!is.null(names(by))) {
    shape_data@data <- dplyr::rename(shape_data@data, !!!rlang::syms(by))
    by <- names(by)
  }
  suffix <- rev(suffix)
  if (direction == "left") direction <- "right"
  if (direction == "right") direction <- "left"

  alligned <- allign_id_vars(shape_data@data, data, by)
  # Use same secret ID variable as the sp package
  # (https://github.com/cran/sp/blob/a7c10d3e1b02db2451ff2bc8435a8518d0b5c692/R/merge.R#L32)
  alligned$data$DoNotUse_temp_sequential_ID_963 <- seq(1, nrow(alligned$data))
  merge_f <- utils::getFromNamespace(paste0(direction, "_join"), "dplyr")
  out <- merge_f(alligned$shape_data, alligned$data, by = by, suffix = suffix)

  if (verbose) {
    join_fail_attributes <- check_for_join_failures(out, by, alligned$shape_data, alligned$data)
  } else {
    join_fail_attributes <- NULL
  }

  out <- dplyr::select(out, dplyr::one_of(names(data)), dplyr::everything())

  # Construct the sp object
  shape_data_out <- shape_data[out$DoNotUse_temp_sequential_ID_963, ]
  out$DoNotUse_temp_sequential_ID_963 <- NULL
  shape_data_out@data <- out
  attr(shape_data_out, "join_failures") <- join_fail_attributes

  shape_data_out
}


check_shape_join_names <- function(by_names, data_names, display) {
  not_avail <- dplyr::setdiff(by_names, data_names)
  if (length(not_avail) > 0) {
    stop(custom_format_text(
      "Variables ", paste(not_avail, collapse = ", "), " are not in ", display, ".",
      indent = 2, exdent = 2
    ))
  }
}


allign_id_vars <- function(shape_data, data, by) {
  shp_id_is_char <- purrr::map_lgl(by, ~is.character(shape_data[[.]]))
  shp_id_is_num <- purrr::map_lgl(by, ~is.numeric(shape_data[[.]]))
  shp_id_is_fact <- purrr::map_lgl(by, ~is.factor(shape_data[[.]]))
  data_id_is_char <- purrr::map_lgl(by, ~is.character(data[[.]]))
  data_id_is_num <- purrr::map_lgl(by, ~is.numeric(data[[.]]))
  data_id_is_fact <- purrr::map_lgl(by, ~is.factor(data[[.]]))

  convert_failures <- rep(FALSE, length(by))
  for (iii in seq_along(by)) {
    # If one is character but other is numeric, convert if possible
    # if one is factor and the other is character, convert to character
    # If one is factor and the other is numeric, give error because I can't
    # really imagine how this happened.
    # TODO: It seems like a lot of people may convert the data from number -> factor
    #       (using the labels) and then try to merge on the "numeric" id (which
    #       is often stored as text in shape file). Consider trying to give
    #       better error in this situation.
    if (shp_id_is_char[iii] && data_id_is_num[iii]) {
      shape_data[[by[iii]]] <- custom_parse_number(shape_data[[by[iii]]])
      if (is.character(shape_data[[by[iii]]])) convert_failures[iii] <- TRUE
    } else if (shp_id_is_num[iii] && data_id_is_char[iii]) {
      data[[by[iii]]] <- custom_parse_number(data[[by[iii]]])
      if (is.character(shape_data[[by[iii]]])) convert_failures[iii] <- TRUE
    } else if (shp_id_is_char[iii] && data_id_is_fact[iii]) {
      data[[by[iii]]] <- as.character(data[[by[iii]]])
    } else if (shp_id_is_fact[iii] && data_id_is_char[iii]) {
      shape_data[[by[iii]]] <- as.character(shape_data[[by[iii]]])
    } else if (shp_id_is_fact[iii] && data_id_is_num[iii]) {
      stop(paste0("Variable ", by[iii], "is factor in shape data but numeric in data."))
    } else if (shp_id_is_num[iii] && data_id_is_fact[iii]) {
      stop(paste0("Variable ", by[iii], "is factor in data but numeric in shape data."))
    }

    if (any(convert_failures)) {
      bad_shape <- by[convert_failures]
      bad_data <- by[convert_failures]
      text <- ifelse(bad_shape != bad_data, paste0(bad_shape, " -> ", bad_data), bad_shape)
      stop(paste0(
        "Variables were numeric in one object but character in the other and ",
        "could not be converted:\n", paste(text, collapse = ", ")
      ))
    }

    #Combine attributes (prioritzing data attributes because the DDI has more info)
    shape_attr <- attributes(shape_data[[by[iii]]])
    data_attr <- attributes(data[[by[iii]]])

    overlapping_attr <- dplyr::intersect(names(shape_attr), names(data_attr))
    shape_attr <- shape_attr[!names(shape_attr) %in% overlapping_attr]

    all_attr <- c(data_attr, shape_attr)
    attributes(shape_data[[by[iii]]]) <- all_attr
    attributes(data[[by[iii]]]) <- all_attr
  }
  list(shape_data = shape_data, data = data)
}

check_for_join_failures <- function(merged, by, shape_data, data) {
  merge_fail <- list(
    shape = dplyr::anti_join(shape_data, as.data.frame(merged), by = by),
    data = dplyr::anti_join(data, merged, by = by)
  )
  sh_num <- nrow(merge_fail$shape)
  d_num <- nrow(merge_fail$data)
  if (sh_num > 0 | d_num > 0) {
    if (sh_num > 0 && d_num > 0) {
      count_message <- paste0(sh_num, " observations in the shape file and ", d_num, " obervation in data")
    } else if (sh_num > 0) {
      count_message <- paste0(sh_num, " observations in the shape file")
    } else if (d_num > 0) {
      count_message <- paste0(d_num, " observations in the data")
    }
    custom_cat(
      "Some observations were lost in the join (", count_message,
      "). See `join_failures(...)` for more details."
    )
    merge_fail
  } else {
    return(NULL)
  }
}


#' Report on observations dropped by a join
#'
#' Helper for learning which observations were dropped from a dataset because
#' they were not joined on.
#'
#' @param join_results A dataset that has just been created by a shape join
#'   (like \code{\link{ipums_shape_left_join}})
#' @return returns a list of data.frames, where the first item (shape) is the observations
#'   dropped from the shape file and the second (data) is the observations dropped from the
#'   data.
#' @export
join_failures <- function(join_results) {
  out <- attr(join_results, "join_failures")
  if (is.null(out)) {
    message("No join failures found.")
    NULL
  } else {
    out
  }
}
