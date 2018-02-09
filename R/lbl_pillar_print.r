#' @importFrom pillar pillar_shaft
#' @export
NULL

#' @export
pillar_shaft.ipums_labelled <- function(x, ...) {
  labels <- attr(x, "labels")

  aligned_vals <- allign_vals(unclass(x))
  out <- aligned_vals


  val <- unname(labels)
  val <- allign_vals(val)
  lbl <- names(labels)

  lbl_display <- paste(val, pillar::style_subtle(paste0("[", lbl, "]")))
  matches <- match(x, unname(labels))
  out[!is.na(matches)] <- lbl_display[matches[!is.na(matches)]]

  max_width <- pillar::get_max_extent(out)
  MIN_LBL_SPACE <- 8
  min_width <- pillar::get_max_extent(aligned_vals)
  if (any(!is.na(matches))) min_width <- min_width + MIN_LBL_SPACE

  pillar::new_pillar_shaft(
    out, min_width = min_width, width = max_width, subclass = "basic_subtle_trunc"
  )
}

allign_vals <- function(x) {
  pad_side <- if (is.numeric(x)) "left" else "right"
  out <- as.character(x)
  out[is.na(out)] <- pillar::style_na("NA")
  wids <- pillar::get_extent(out)

  padding <- stringr::str_dup(" ", max(wids) - wids)

  if (pad_side == "left") {
    out <- paste0(padding, out)
  } else {
    out <- paste0(out, padding)
  }
  out
}


# Adapted from pillar because I want to have subtle elipses at the end
#' @export
format.basic_subtle_trunc <- function(x, width, ...) {
  desired_width <- attr(x, "width")
  data <- x
  if (width < desired_width) {
    data <- subtle_str_trunc(data, width)
  }
  data[is.na(data)] <- pillar::style_na("NA")

  pillar::new_ornament(data, width = width, align = "left")
}

subtle_str_trunc <- function(x, width) {
  if (is.infinite(width)) return(x)
  str_width <- pillar::get_extent(x)
  too_wide <- which(!is.na(x) & str_width > width)
  x[too_wide] <- paste0(crayon::col_substr(
    x[too_wide], 1, width - 1), pillar::style_subtle(cli::symbol$continue))
  x
}


#' @export
`[.ipums_labelled` <- function(x, ...) {
  ipums_labelled(NextMethod())
}

#' Create the ipums_labelled class
#'
#' To get nice printing in the pillar package we had to add an extra class
#' to the labelled class. In normal use, you shouldn't need to worry about
#' this function at all.
#'
#' @param x A vector
#' @param ... Other parameters passed to the methods
#'
#' @keywords internal
#' @export
ipums_labelled <- function(x, ...) {
  UseMethod("ipums_labelled")
}

#' @export
ipums_labelled.default <- function(x, labelled, ...) {
  ipums_labelled(haven::labelled(x, labelled, ...))
}

#' @export
ipums_labelled.labelled <- function(x, ...) {
  class(x) <- c("ipums_labelled", class(x))
  x
}
