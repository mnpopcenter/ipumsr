# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

# Implement pillar printing for haven_labelled objects - can
# be deleted if/when https://github.com/tidyverse/haven/pull/390
# or equivalent is merged

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.haven_labelled <- function(
  x,
  show_labels = getOption("ipumsr.show_pillar_labels", TRUE),
  ...
) {
  if (!isTRUE(show_labels)) {
    return(pillar::pillar_shaft(unclass(x)))
  }
  val <- val_pillar_info(x)
  na <- na_pillar_info(x)
  lbl <- lbl_pillar_info(x)

  pillar::new_pillar_shaft(
    list(val, na, lbl),
    min_width = max(val$wid_short + na$wid_full + lbl$wid_short),
    width =  max(val$wid_full + na$wid_full + lbl$wid_full),
    subclass = "pillar_shaft_haven_labelled"
  )
}

#' @export
pillar_shaft.labelled <- pillar_shaft.haven_labelled

#' @export
format.pillar_shaft_haven_labelled <- function(x, width, ...) {
  pillar::new_ornament(
    truncate_labelled_pillar(x[[1]], x[[2]], x[[3]], attr(x, "width"), width),
    width = width,
    align = "left"
  )
}

str_trunc <- function(x, widths, subtle = FALSE) {
  str_width <- pillar::get_extent(x)
  too_wide <- which(!is.na(x) & str_width > widths)

  continue_symbol <- cli::symbol$continue
  if (subtle) continue_symbol <- pillar::style_subtle(continue_symbol)

  truncated <- Map(x[too_wide], widths[too_wide], f = function(item, wid) {
    paste0(crayon::col_substr(item, 1, wid - 1), continue_symbol)
  })
  truncated <- as.vector(truncated, "character")
  x[too_wide] <- truncated

  x
}

val_pillar_info <- function(x) {
  MIN_CHR_DISPLAY <- 4
  non_spss_x <- x
  class(non_spss_x) <- setdiff(class(non_spss_x), c("haven_labelled_spss", "labelled_spss"))
  val_pillar <- pillar::pillar_shaft(haven::zap_labels(non_spss_x))
  if (is.numeric(x)) {
    disp_short <- trim_ws_rhs(format(val_pillar, attr(val_pillar, "min_width")))
    disp_full <- trim_ws_rhs(format(val_pillar, attr(val_pillar, "width")))

    ret <- list(
      type = "numeric",
      disp_short = disp_short,
      wid_short = pillar::get_extent(disp_short),
      disp_full = disp_full,
      wid_full = pillar::get_extent(disp_full)
    )
  } else {
    disp_full <- trim_ws_rhs(format(val_pillar, attr(val_pillar, "width")))
    wid_full <- pillar::get_extent(disp_full)

    ret <- list(
      type = "character",
      val_pillar = val_pillar,
      wid_short = pmin(MIN_CHR_DISPLAY, wid_full),
      disp_full = disp_full,
      wid_full = wid_full
    )
  }
  ret
}

na_pillar_info <- function(x) {
  na_display <- character(length(x))
  if (is.double(x)) {
    na_display[haven::is_tagged_na(x)] <- paste0("(", haven::na_tag(x[haven::is_tagged_na(x)]), ")")
  }
  if (inherits(x, "labelled_spss")) {
    is_spss_na <- is.na(x) & !is.na(unclass(x))
    na_display[is_spss_na] <- "(NA)"
    class(x) <- setdiff(class(x), "")
  }
  na_display <- pillar::style_na(na_display)
  na_display_lens <- pillar::get_extent(na_display)
  ret <- list(
    disp_full = na_display,
    wid_full = na_display_lens
  )
  ret
}

lbl_pillar_info <- function(x) {
  MIN_LBL_DISPLAY <- 6
  labels <- attr(x, "labels")
  if (length(labels) > 0) {
    names(labels) <- pillar::style_subtle(paste0(" [", names(labels), "]"))
    attr(x, "labels") <- labels
    label_display <- as.character(as_factor(x, "labels"))
    label_display[is.na(label_display)] <- ""
  } else {
    label_display <- character(length(x))
  }
  label_widths <- pillar::get_extent(label_display)
  label_min_widths <- ifelse(label_widths > 0, pmin(MIN_LBL_DISPLAY, label_widths), 0)

  ret <- list(
    wid_short = label_min_widths,
    disp_full = label_display,
    wid_full = label_widths
  )
  ret
}

truncate_labelled_pillar <- function(vals, nas, lbls, desired_width, width) {
  if (width >= desired_width) {
    return(paste0(vals$disp_full, nas$disp_full, lbls$disp_full))
  }
  if (vals$type == "numeric") {
    if (width < max(vals$wid_full + nas$wid_full + lbls$wid_short)) {
      val_display <- vals$disp_short
      lbls_display <- str_trunc(lbls$disp_full, width - nas$wid_full - vals$wid_short, subtle = TRUE)
    } else {
      val_display <- vals$disp_full
      lbls_display <- str_trunc(lbls$disp_full, width - nas$wid_full - vals$wid_full, subtle = TRUE)
    }
  } else {
    val_widths <- pmin(vals$wid_full, width - nas$wid_full - lbls$wid_short)
    val_display <- str_trunc(vals$disp_full, val_widths)

    lbls_display <- str_trunc(lbls$disp_full, width - nas$wid_full - val_widths, subtle = TRUE)
  }
  paste0(val_display, nas$disp_full, lbls_display)
}

trim_ws_rhs <- function(x) {
  sub("[[:blank:]]+$", "", x)
}
