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
