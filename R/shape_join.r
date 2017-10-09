ipums_shape_left_join <- function(data, shape_data, by, suffix, verbose) {
  ipums_shape_join(data, shape_data, by, "left", suffix, verbose)
}

ipums_shape_right_join <- function(data, shape_data, by, suffix, verbose) {
  ipums_shape_join(data, shape_data, by, "right", suffix, verbose)
}

ipums_shape_inner_join <- function(data, shape_data, by, suffix, verbose) {
  ipums_shape_join(data, shape_data, by, "inner", suffix, verbose)
}

ipums_shape_full_join <- function(data, shape_data, by, suffix, verbose) {
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
  if (!is.null(names(by))) {
    by_shape <- by
    by_data <- by
  } else {
    by_shape <- names(by)
    by_data <- unname(by)
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

  # message for merge failures
  if (verbose) {
    join_fail_attributes <- check_for_join_failures(out, by)
    attr(out, "join_failures") <- join_fail_attributes
  }

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
  if (!is.null(names(by))) {
    by_shape <- by
    by_data <- by
  } else {
    by_shape <- names(by)
    by_data <- unname(by)
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
    join_fail_attributes <- check_for_join_failures(out, by)
  } else {
    join_fail_attributes <- NULL
  }

  # Construct the sp object
  shape_data_out <- shape_data[out$DoNotUse_temp_sequential_ID_963]
  out$DoNotUse_temp_sequential_ID_963 <- NULL
  shape_data_out@data <- out
  attr(out_shape_data, "join_failures") <- join_fail_attributes

  shape_data_out
}


check_shape_join_names <- function(by_names, data_names, display) {
  not_avail <- dplyr::setdiff(by_names, data_names)
  if (length(not_avail) > 0) {
    stop(paste0("Variables ", paste(not_avail, collapse = ", "), " are not in ", display, "."))
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
      shape_data[[by[iii]]] <- as.character(shape_data[[by[iii]]])
    } else if  (shp_id_is_fact[iii] && data_id_is_char[iii]) {
      data[[by[iii]]] <- as.character(data[[by[iii]]])
    } else if (shp_id_is_fact[iii] && data_id_is_num[iii]) {
      stop(paste0("Variable ", by[iii], "is factor in shape data but numeric in data."))
    } else if (shp_id_is_num[iii] && data_id_is_fact[iii]) {
      stop(paste0("Variable ", by[iii], "is factor in data but numeric in shape data."))
    }

    if (any(convert_failures)) {
      bad_shape <- by_shape[convert_failures]
      bad_data <- by_data[convert_failures]
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

check_for_join_failures <- function(merged, by) {
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
    cat(paste0(
      "Some observations were lost in the join (", count_message, "). See `join_problems(...)` for more details."
    ))
    merge_fail
  } else {
    return(NULL)
  }
}
