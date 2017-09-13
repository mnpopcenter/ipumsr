# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' Set labelled values to missing
#'
#' Convert values to NA based on their label and value in a
#' \code{\link[haven]{labelled}} vector. Ignores any value that does not have a
#' label.
#'
#' @param x A \code{\link[haven]{labelled}} vector
#' @param .predicate A function that takes .val and .lbl (the values and
#'    labels) and returns TRUE or FALSE. It is passed to a function similar
#'    to \code{\link[rlang]{as_function}}, so also accepts quosure-style lambda
#'    functions (that use values .val and .lbl).
#' @return A haven::labeled vector
#' @examples
#' x <- haven::labelled(
#'   c(10, 10, 11, 20, 30, 99, 30, 10),
#'   c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
#' )
#'
#' lbl_na_if(x, ~.val >= 90)
#' lbl_na_if(x, ~.lbl %in% c("Maybe"))
#' lbl_na_if(x, ~.val >= 90 | .lbl %in% c("Maybe"))
#'
#' # You can also use the more explicit function notation
#' lbl_na_if(x, function(.val, .lbl) .val >= 90)
#'
#' # Or even the name of a function
#' na_function <- function(.val, .lbl) .val >= 90
#' lbl_na_if(x, "na_function")
#'
#' @family lbl_helpers
#' @export
lbl_na_if <- function(x, .predicate) {
  pred_f <- as_lbl_function(.predicate, caller_env())

  labels <- attr(x, "labels")
  to_zap <- pred_f(.val = unname(labels), .lbl = names(labels))

  if (any(is.na(to_zap))) {
    stop("Predicates cannot evaluate to missing in `lbl_na_if()`.", call. = FALSE)
  }

  vals_to_zap <- unname(labels[to_zap])
  new_labels <- labels[!to_zap]

  out <- x
  out[out %in% vals_to_zap] <- NA
  attr(out, "labels") <- new_labels

  out
}

#' Collapse labelled values to labels that already exist
#'
#' Converts values to a new value based on their label and value in a
#' \code{\link[haven]{labelled}} vector. If the newly assigned value does
#' not match an already existing labelled value, the smallest value's label
#' is used. Ignores any value that does not have a label.
#'
#' @param x A \code{\link[haven]{labelled}} vector
#' @param .fun A function that takes .val and .lbl (the values and
#'    labels) and returns the values of the label you want to change it to.
#'    It is passed to a function similar to \code{\link[rlang]{as_function}}, so
#'    also accepts quosure-style lambda functions (that use values .val and .lbl).
#' @return A haven::labeled vector
#' @examples
#' x <- haven::labelled(
#'   c(10, 10, 11, 20, 30, 99, 30, 10),
#'   c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
#' )
#'
#' lbl_collapse(x, ~(.val %/% 10) * 10)
#' # Notice that 90 get's NIU from 99 even though 90 didn't have a label in original
#'
#' lbl_collapse(x, ~ifelse(.lbl == 10, 11, .lbl))
#' # But here 10 is assigned 11's label
#'
#' # You can also use the more explicit function notation
#' lbl_collapse(x, function(.val, .lbl) (.val %/% 10) * 10)
#'
#' # Or even the name of a function
#' collapse_function <- function(.val, .lbl) (.val %/% 10) * 10
#' lbl_na_if(x, "collapse_function")
#'
#' @family lbl_helpers
#' @export
lbl_collapse <- function(x, .fun) {
  pred_f <- as_lbl_function(.fun, caller_env())

  old_attributes <- attributes(x)

  label_info <- dplyr::data_frame(
    old_val = unname(old_attributes$labels),
    old_label = names(old_attributes$labels),
    new_val = pred_f(.val = .data$old_val, .lbl = .data$old_label),
    vals_equal = .data$old_val == .data$new_val
  )
  # Arrange so that if value existed in old values it is first, otherwise first old value
  label_info <- dplyr::group_by(label_info, .data$new_val)
  label_info <- dplyr::arrange(label_info, .data$new_val, dplyr::desc(.data$vals_equal), .data$old_val)
  label_info <- dplyr::mutate(label_info, new_label = .data$old_label[1])
  label_info <- dplyr::ungroup(label_info)

  new_labels <- dplyr::select(label_info, dplyr::one_of(c("new_label", "new_val")))
  new_labels <- dplyr::distinct(new_labels)
  new_labels <- tibble::deframe(new_labels)
  new_attributes <- old_attributes
  new_attributes$labels <- new_labels

  out <- label_info$new_val[match(x, label_info$old_val)]

  attributes(out) <- new_attributes
  out
}

#' Relabel labelled values
#'
#' Converts values to a new value (that may or may not exist) based on their
#' label and value in a \code{\link[haven]{labelled}} vector. Ignores any value
#' that does not have a label.
#'
#' @param x A \code{\link[haven]{labelled}} vector
#' @param ... Two-sided formulas where the left hand side is a label placeholder
#'   (created with the \code{\link{lbl}} function) or a value that already exists
#'   in the data and the right hand side is a function that returns a logical
#'   vector that indicates which labels should be relabeled. The right hand side
#'   is passed to a function similar to \code{\link[rlang]{as_function}}, so
#'   also accepts quosure-style lambda functions (that use values .val and .lbl).
#' @return A haven::labeled vector
#' @examples
#' x <- haven::labelled(
#'   c(10, 10, 11, 20, 30, 99, 30, 10),
#'   c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
#' )
#'
#' lbl_relabel(
#'   x,
#'   lbl(10, "Yes/Yes-ish") ~ .val %in% c(10, 11),
#'   lbl(90, "???") ~ .val == 99 | .lbl == "Maybe"
#' )
#'
#' # If relabelling to labels that already exist, don't need to specify both label
#' # and value:
#' # If just bare, assumes it is a value:
#' lbl_relabel(x, 10 ~ .val == 11)
#' # Use single argument to lbl for the label
#' lbl_relabel(x, lbl("Yes") ~ .val == 11)
#' # Or can used named arguments
#' lbl_relabel(x, lbl(.val = 10) ~ .val == 11)
#'
#' @family lbl_helpers
#' @export
lbl_relabel <- function(x, ...) {
  dots <- list(...)

  purrr::reduce(dots, .init = x, function(.x, .y) {
    old_labels <- attr(.x, "labels")
    # Figure out which values we're changing
    ddd <- .y
    rlang::f_lhs(ddd) <- NULL
    to_change <- as_lbl_function(ddd)(.val = unname(old_labels), .lbl = names(old_labels))

    # Figure out which label we're changing to
    ddd <- .y
    rlang::f_rhs(ddd) <- rlang::f_lhs(ddd)
    rlang::f_lhs(ddd) <- NULL
    lblval <- rlang::eval_tidy(rlang::as_quosure(ddd))
    lblval <- fill_in_lbl(lblval, old_labels)

    # Make changes to vector
    out <- .x
    out[out %in% old_labels[to_change]] <- lblval$.val

    new_labels <- dplyr::data_frame(
      label <- c(names(old_labels[!to_change]), lblval$.lbl),
      value = c(unname(old_labels[!to_change]), lblval$.val)
    )
    new_labels <- dplyr::distinct(new_labels)
    new_labels <- dplyr::arrange(new_labels, .data$value)
    new_labels <- tibble::deframe(new_labels)

    attr(out, "labels") <- new_labels
    out
  })
}


# Based on rlang::as_function
# Changed so that instead of function having args .x & .y, it has
# .val and .lbl
as_lbl_function <- function(x, env = caller_env()) {
  rlang::coerce_type(
    x,
    rlang::friendly_type("function"),
    primitive = ,
    closure = {
      x
    },
    formula = {
      if (length(x) > 2) {
        abort("Can't convert a two-sided formula to a function")
      }
      args <- list(... = rlang::missing_arg(), .val = quote(..1), .lbl = quote(..2))
      rlang::new_function(args, rlang::f_rhs(x), rlang::f_env(x))
    },
    string = {
      get(x, envir = env, mode = "function")
    }
  )
}

#' Make a label placeholder object
#'
#' Helper to make a placeholder for a label-value pair.
#' @param ... Either one or two arugments, possibly named .val and .lbl. If a
#'   single unnamed value, represents the label, if 2 unnamed values, the first
#'   is the value and the second is the label.
#' @family lbl_helpers
#' @export
lbl <- function(...) {
  dots <- list(...)

  if (!is.null(names(dots)) && any(!names(dots) %in% c(".val", ".lbl", ""))) {
    stop("Expected only arguments named `.lbl` and `.val`")
  }

  if (length(dots) == 1) {
    if (!is.null(names(dots)) && names(dots) == ".val") {
      out <- list(.val = dots[[1]], .lbl = NULL)
    } else {
      out <- list(.val = NULL, .lbl = dots[[1]])
    }
  } else if (length(dots) == 2) {
    if (is.null(names(dots))) {
      names(dots) <- c(".val", ".lbl")
    } else {
      named_val <- names(dots) == ".val"
      named_lbl <- names(dots) == ".lbl"
      if (any(named_val)) names(dots)[!named_val] <- ".lbl"
      if (any(named_lbl)) names(dots)[!named_lbl] <- ".val"
    }
    out <- list(.val = dots[[".val"]], .lbl = dots[[".lbl"]])
  } else {
    stop("Expected either 1 or 2 arguments.")
  }

  class(out) <- "lbl_placeholder"
  out
}


fill_in_lbl <- function(lblval, orig_labels) {
  if (class(lblval) != "lbl_placeholder") lblval <- lbl(.val = lblval)
  if (is.null(lblval$.lbl) & is.null(lblval$.val)) {
    stop("Could not fill in label because neither label nor value is specified")
  }
  if (is.null(lblval$.lbl)) {
    found_val <- unname(orig_labels) == lblval$.val
    if (!any(found_val)) stop(paste0("Could not find value ", lblval$.val,  " in existing labels."))
    lblval$.lbl <- names(orig_labels)[found_val]
  }
  if (is.null(lblval$.val)) {
    found_lbl <- names(orig_labels) == lblval$.lbl
    if (!any(found_lbl)) stop(paste0("Could not find label ", lblval$.lbl,  " in existing labels."))
    lblval$.val <- unname(orig_labels)[found_lbl]
  }
  lblval
}
