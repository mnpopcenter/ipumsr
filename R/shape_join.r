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

  not_in_shape <- dplyr::setdiff(by_shape, names(shape_data))
  if (length(not_in_shape) > 0) {
    stop(paste0("Variables ", paste(not_in_shape, collapse = ", "), " are not in shape data."))
  }
  not_in_data <- dplyr::setdiff(by_shape, names(data))
  if (length(not_in_shape) > 0) {
    stop(paste0("Variables ", paste(not_in_data, collapse = ", "), " are not in data."))
  }
  direction <- match.arg(direction)

  # We're pretending like the x in the join is the data, but
  # because of the join functions dispatch, we will actually be
  # doing the reverse. Therefore, rename the variables in shape,
  # and also reverse the suffix.
  if (!is.null(names(by))) {
    shape_data <- dplyr::rename(shape_data, !!!rlang::syms(by))
    by <- names(by)
  }
  suffix <- rev(suffix)

  # Allign variable attributes
  ## Vectors to hold type information
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
    #       (using the labels) and then try to merge on the "numeirc" id (which
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

  merge_f <- utils::getFromNamespace(paste0(direction, "_join"), "dplyr")
  out <- merge_f(shape_data, data, by = by, suffix = suffix)

  # message for merge failures
  if (verbose) {
    merge_fail <- list(
      shape = dplyr::anti_join(shape_data, as.data.frame(out), by = by),
      data = dplyr::anti_join(data, out, by = by)
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
      attr(out, "join_problems") <- merge_fail
    }
  }
  out
}
