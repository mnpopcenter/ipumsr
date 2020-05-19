# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

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
#'    functions (that use values .val and .lbl). See examples for more information.
#' @return A haven::labelled vector
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
#'    See examples for more information.
#' @return A haven::labelled vector
#' @examples
#' x <- haven::labelled(
#'   c(10, 10, 11, 20, 30, 99, 30, 10),
#'   c(Yes = 10, `Yes - Logically Assigned` = 11, No = 20, Maybe = 30, NIU = 99)
#' )
#'
#' lbl_collapse(x, ~(.val %/% 10) * 10)
#' # Notice that 90 get's NIU from 99 even though 90 didn't have a label in original
#'
#' lbl_collapse(x, ~ifelse(.val == 10, 11, .val))
#' # But here 10 is assigned 11's label
#'
#' # You can also use the more explicit function notation
#' lbl_collapse(x, function(.val, .lbl) (.val %/% 10) * 10)
#'
#' # Or even the name of a function
#' collapse_function <- function(.val, .lbl) (.val %/% 10) * 10
#' lbl_collapse(x, "collapse_function")
#'
#' @family lbl_helpers
#' @export
lbl_collapse <- function(x, .fun) {
  pred_f <- as_lbl_function(.fun, caller_env())

  old_attributes <- attributes(x)

  label_info <- tibble::tibble(
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
#'   See examples for more information.
#' @return A haven::labelled vector
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
  if (is.null(attr(x, "labels", exact = TRUE))) {
    stop("Vector must be labelled. To add labels to an unlabelled vector use ",
         "'lbl_define()'.")
  }
  dots <- list(...)

  transformation <- ipums_val_labels(x)
  transformation$new_val <- transformation$val
  transformation$new_lbl <- transformation$lbl
  old_labels <- attr(x, "labels")

  for (ddd in dots) {
    # Figure out which values we're changing from rhs
    ddd_rhs <- ddd
    rlang::f_lhs(ddd_rhs) <- NULL
    to_change <- as_lbl_function(ddd_rhs)(.val = transformation$val, .lbl = transformation$lbl)

    # Figure out which label we're changing to from lhs
    ddd_lhs <- ddd
    rlang::f_rhs(ddd_lhs) <- NULL
    lblval <- rlang::eval_tidy(rlang::as_quosure(ddd_lhs))
    lblval <- fill_in_lbl(lblval, old_labels)

    transformation$new_val[to_change] <- lblval$.val
    transformation$new_lbl[to_change] <- lblval$.lbl
  }

  new_lbls <- dplyr::distinct(transformation, val = .data$new_val, lbl = .data$new_lbl)
  lbl_count <- table(new_lbls$val)

  if (any(lbl_count > 1)) {
    dup_lbls <- new_lbls[new_lbls$val %in% names(lbl_count)[lbl_count > 1], ]
    dup_lbls <- dplyr::group_by(dup_lbls, .data$val)
    dup_lbls <- dplyr::summarize(dup_lbls, all_lbls = paste0("'", lbl, "'", collapse = ", "))

    stop(paste0(
      "Some values have more than 1 label:\n",
      custom_format_text(
        paste(dup_lbls$val, "->", dup_lbls$all_lbls, collapse = "\n"),
        indent = 2, exdent = 2
      )
    ), call. = FALSE)
  }

  new_lbls <- dplyr::arrange(new_lbls, .data$val)
  new_lbls <- purrr::set_names(new_lbls$val, new_lbls$lbl)

  out <- transformation$new_val[match(x, transformation$val)]
  attributes(out) <- attributes(x)
  attr(out, "labels") <- new_lbls
  out
}


#' Define labels for an unlabelled vector
#'
#' Creates a \code{\link[haven]{labelled}} vector from an unlabelled atomic
#' vector using \code{\link{lbl_relabel}} syntax, which allows grouping multiple
#' values into a single labelled value. Values not assigned a label will remain
#' unlabelled.
#' @param x An unlabelled atomic vector
#' @param ... Two-sided formulas where the left hand side is a label placeholder
#'   (created with the \code{\link{lbl}} function) and the right hand side is a
#'   function that returns a logical vector that indicates which existing values
#'   should be assigned that labeled value. The right hand side is passed to a
#'   function similar to \code{\link[rlang]{as_function}}, so also accepts
#'   quosure-style lambda functions (that use values .val and .lbl). See
#'   examples for more information.
#' @return A haven::labelled vector
#' @examples
#' age <- c(10, 12, 16, 18, 20, 22, 25, 27)
#'
#' # Note that values not assigned a new labelled value remain unchanged
#' lbl_define(
#'   age,
#'   lbl(1, "Pre-college age") ~ .val < 18,
#'   lbl(2, "College age") ~ .val >= 18 & .val <= 22
#' )
#' @family lbl_helpers
#' @export
lbl_define <- function(x, ...) {
  if (!is.null(attr(x, "labels", exact = TRUE))) {
    stop("Vector should not have labels. To relabel a labelled vector use",
         "'lbl_relabel()'.")
  }
  unique_x <- sort(unique(x))
  tmp_lbls <- rep(NA_character_, length(unique_x))
  attr(x, "labels") <- purrr::set_names(unique_x, tmp_lbls)
  x <- lbl_relabel(x, ...)
  label_is_na <- is.na(names(attr(x, "labels")))

  haven::labelled(x, labels = attr(x, "labels")[!label_is_na])
}


#' Add labels for unlabelled values
#'
#' Add labels for values that don't already have them.
#'
#' @param x A \code{\link[haven]{labelled}} vector
#' @param ... Labels formed by \code{\link{lbl}} indicating the value and label to be added.
#' @param vals Vector of values to be labelled. NULL, the default labels all values
#'   that are in the data, but aren't already labelled.
#' @param labeller A function that takes a single argument of the values and returns the
#'   labels. Defaults to \code{as.character}. \code{\link[rlang]{as_function}}, so
#'   also accepts quosure-style lambda functions. See examples for more details.
#' @return A haven::labelled vector
#' @examples
#' x <- haven::labelled(
#'   c(100, 200, 105, 990, 999, 230),
#'   c(`Unknown` = 990, NIU = 999)
#' )
#'
#' lbl_add(x, lbl(100, "$100"), lbl(105, "$105"), lbl(200, "$200"), lbl(230, "$230"))
#'
#' lbl_add_vals(x)
#' lbl_add_vals(x, ~paste0("$", .))
#' lbl_add_vals(x, vals = c(100, 200))
#'
#' @family lbl_helpers
#' @export
lbl_add <- function(x, ...) {
  dots <- list(...)

  purrr::reduce(dots, .init = x, function(.x, .y) {
    old_labels <- attr(.x, "labels")

    # Figure out which label we're changing to
    lblval <- fill_in_lbl(.y, old_labels)

    # Make changes to vector
    out <- .x

    new_labels <- tibble::tibble(
      label <- c(names(old_labels), lblval$.lbl),
      value = c(unname(old_labels), lblval$.val)
    )
    new_labels <- dplyr::distinct(new_labels)

    dup_labels <- table(new_labels$value)
    dup_labels <- names(dup_labels)[dup_labels > 1]
    if (length(dup_labels) > 0) {
      dup_labels <- paste(dup_labels, collapse = ", ")
      stop(paste0(
        "Some values have more than 1 label:\n",
        custom_format_text(dup_labels, indent = 2, exdent = 2)
      ))
    }

    new_labels <- dplyr::arrange(new_labels, .data$value)
    new_labels <- tibble::deframe(new_labels)

    attr(out, "labels") <- new_labels
    out
  })
}

#' @export
#' @rdname lbl_add
lbl_add_vals <- function(x, labeller = as.character, vals = NULL) {
  old_labels <- attr(x, "labels")
  old_labels <- tibble::tibble(val = unname(old_labels), lbl = names(old_labels))

  if (is.null(vals)) {
    vals <- dplyr::setdiff(unique(x), old_labels$val)
  } else {
    if (any(vals %in% unname(old_labels))) {
      stop(paste0("Some values have more than 1 label."))
    }
  }
  new_labels <- tibble::tibble(
    val = vals,
    lbl = purrr::map_chr(vals, rlang::as_function(labeller))
  )

  new_labels <- dplyr::bind_rows(old_labels, new_labels)
  new_labels <- dplyr::arrange(new_labels, .data$val)
  new_labels <- purrr::set_names(new_labels$val, new_labels$lbl)

  out <- x
  attr(out, "labels") <- new_labels
  out
}


#' Clean unused labels
#'
#' Remove labels that do not appear in the data.
#'
#' @param x A \code{\link[haven]{labelled}} vector
#' @return A haven::labelled vector
#' @examples
#' x <- haven::labelled(
#'   c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   c(Q1 = 1, Q2 = 2, Q3 = 3, Q4= 4)
#' )
#'
#' lbl_clean(x)
#'
#' @family lbl_helpers
#' @export
lbl_clean <-function(x) {
  old_labels <- attr(x, "labels")
  unused_labels <- unname(old_labels) %in% dplyr::setdiff(unname(old_labels), unique(unname(x)))

  out <- x
  attr(out, "labels") <- old_labels[!unused_labels]
  out
}

# Based on rlang::as_function
# Changed so that instead of function having args .x & .y, it has
# .val and .lbl
as_lbl_function <- function(x, env = caller_env()) {
  if (rlang::is_function(x)) {
    return(x)
  }

  if (rlang::is_quosure(x)) {
    return(eval(rlang::expr(function(...) rlang::eval_tidy(!!x))))
  }

  if (rlang::is_formula(x)) {
    if (length(x) > 2) {
      rlang::abort("Can't convert a two-sided formula to a function")
    }

    args <- list(... = rlang::missing_arg(), .val = quote(..1), .lbl = quote(..2))
    fn <- new_function(args, rlang::f_rhs(x), rlang::f_env(x))
    return(fn)
  }

  if (rlang::is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }

  abort_coercion_function(x)
}

# Adapted from rlang:::abort_coercion
abort_coercion_function <- function(x) {
  x_type <- friendly_type_of(x)
  abort(paste0("Can't convert ", x_type, " to function"))
}

# Copied from rlang:::friendly_type_of()
friendly_type_of <- function(x, length = FALSE) {
  if (is.object(x)) {
    return(sprintf("a `%s` object", paste(class(x),
                                          collapse = "/")))
  }
  friendly <- as_friendly_type(typeof(x))
  if (length && rlang::is_vector(x)) {
    friendly <- paste0(friendly, sprintf(" of length %s",
                                         length(x)))
  }
  friendly
}

# Copied from rlang:::as_friendly_type()
as_friendly_type <- function(type) {
  switch(type, logical = "a logical vector", integer = "an integer vector",
         numeric = , double = "a double vector", complex = "a complex vector",
         character = "a character vector", raw = "a raw vector",
         string = "a string", list = "a list", `NULL` = "NULL",
         environment = "an environment", externalptr = "a pointer",
         weakref = "a weak reference", S4 = "an S4 object",
         name = , symbol = "a symbol", language = "a call",
         pairlist = "a pairlist node", expression = "an expression vector",
         quosure = "a quosure", formula = "a formula",
         char = "an internal string", promise = "an internal promise",
         ... = "an internal dots object", any = "an internal `any` object",
         bytecode = "an internal bytecode object", primitive = ,
         builtin = , special = "a primitive function", closure = "a function",
         type)
}

#' Make a label placeholder object
#'
#' Helper to make a placeholder for a label-value pair.
#' @param ... Either one or two arguments, possibly named .val and .lbl. If a
#'   single unnamed value, represents the label, if 2 unnamed values, the first
#'   is the value and the second is the label.
#' @return A \code{label_placeholder} object, useful in functions like \code{\link{lbl_add}}
#' @examples
#' x <- haven::labelled(
#'   c(100, 200, 105, 990, 999, 230),
#'   c(`Unknown` = 990, NIU = 999)
#' )
#'
#' lbl_add(x, lbl(100, "$100"), lbl(105, "$105"), lbl(200, "$200"), lbl(230, "$230"))
#'
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
    if (!any(found_val)) {
      stop(paste0("Could not find value ", lblval$.val,  " in existing labels."))
    }
    lblval$.lbl <- names(orig_labels)[found_val]
  }
  if (is.null(lblval$.val)) {
    found_lbl <- names(orig_labels) == lblval$.lbl
    if (!any(found_lbl)) {
      stop(paste0("Could not find label ", lblval$.lbl,  " in existing labels."))
    }
    lblval$.val <- unname(orig_labels)[found_lbl]
  }
  lblval
}

#' Remove all IPUMS attributes from a variable (or all variables in a data.frame)
#'
#' Helper to remove ipums attributes (including value labels from the
#' labelled class, the variable label and the variable description).
#' These attributes can sometimes get in the way of functions like
#' the dplyr join functions so you may want to remove them.
#' @param x A variable or a whole data.frame to remove attributes from
#' @return A variable or data.frame
#' @examples
#' cps <- read_ipums_micro(ipums_example("cps_00006.xml"))
#' annual_unemployment <- data.frame(YEAR = c(1962, 1963), unemp = c(5.5, 5.7))
#'
#' # Avoids warning 'Column `YEAR` has different attributes on LHS and RHS of join'
#' cps$YEAR <- zap_ipums_attributes(cps$YEAR)
#' cps <- dplyr::left_join(cps, annual_unemployment, by = "YEAR")
#'
#' @family lbl_helpers
#' @export
zap_ipums_attributes <- function(x) {
  UseMethod("zap_ipums_attributes")
}

#' @export
zap_ipums_attributes.default <- function(x) {
  x <- zap_labels(x)
  attr(x, "label") <- NULL
  attr(x, "var_desc") <- NULL
  x
}

#' @export
zap_ipums_attributes.data.frame <- function(x) {
  for (iii in seq_len(ncol(x))) {
    x[[iii]] <- zap_ipums_attributes(x[[iii]])
  }
  x
}
