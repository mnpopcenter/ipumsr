# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

ddi_filter_vars <- function(ddi, vars, out_type, verbose) {
  if (rlang::quo_is_null(vars)) {
    return(ddi)
  }
  selected_vars <-dplyr::select_vars(ddi$var_info$var_name, !!vars)

  if (ddi$file_type == "hierarchical" & out_type == "list") {
    key_vars <- purrr::flatten_chr(ddi$rectypes_keyvars$keyvars)
    missing_kv <- dplyr::setdiff(key_vars, selected_vars)

    if (length(missing_kv) > 0) {
      if (verbose) {
        cat(paste0(
          "Adding cross rectype linking vars ('",
          paste(missing_kv, collapse = "', '"),
          "') to data.\n\n"
        ))
      }
      selected_vars <- c(selected_vars, missing_kv)
    }
  } else if (ddi$file_type == "hierarchical" & out_type == "long") {
    if (!ddi$rectype_idvar %in% selected_vars) {
      if (verbose) {
        cat(paste0("Adding rectype id var '", ddi$rectype_idvar, "' to data.\n\n"))
      }
      selected_vars <- c(selected_vars, ddi$rectype_idvar)
    }
  }

  ddi$var_info <- dplyr::filter(ddi$var_info, .data$var_name %in% selected_vars)
  ddi
}

ddi_to_rtinfo <- function(ddi) {
  if (ddi$file_type == "rectangular") {
    out <- hipread::hip_rt(1, 0)
  } else if (ddi$file_type == "hierarchical") {
    rec_vinfo <- dplyr::filter(ddi$var_info, .data$var_name == ddi$rectype_idvar)
    if (nrow(rec_vinfo) > 1) stop("Cannot support multiple rectype id variables.", call. = FALSE)
    out <- hipread::hip_rt(rec_vinfo$start, rec_vinfo$end - rec_vinfo$start + 1)
  } else {
    stop(paste0("Unexpected file type: ", ddi$file_type))
  }
  out
}

ddi_to_colspec <- function(ddi, out_type, verbose) {
  if (ddi$file_type == "rectangular") {
    out <- hipread::hip_fwf_positions(
      ddi$var_info$start,
      ddi$var_info$end,
      ddi$var_info$var_name,
      hipread_type_name_convert(ddi$var_info$var_type),
      imp_dec = ddi$var_info$imp_decim
    )

    if (out_type == "list") {
      if (verbose) cat("Assuming data rectangularized to 'P' record type")
      out <- list("P" = out)
    }
  } else if (ddi$file_type == "hierarchical") {
    col_info <- tidyr::unnest_(ddi$var_info, "rectypes", .drop = FALSE)

    rts <- unique(col_info$rectypes)
    out <- purrr::map(rts, function(rt) {
      rt_cinfo <- col_info[col_info$rectypes == rt, ]
      hipread::hip_fwf_positions(
        rt_cinfo$start,
        rt_cinfo$end,
        rt_cinfo$var_name,
        hipread_type_name_convert(rt_cinfo$var_type),
        imp_dec = rt_cinfo$imp_decim
      )
    })
    names(out) <- rts
  } else {
    stop(paste0("Unexpected file type: ", ddi$file_type))
  }
  out
}

ddi_to_readr_colspec <- function(ddi) {
  col_types <- purrr::map(ddi$var_info$var_type, function(x) {
    if (x == "numeric") out <- readr::col_double()
    else if(x == "character") out <- readr::col_character()
    else if (x == "integer") out <- readr::col_integer()
    out
  })
  names(col_types) <- ddi$var_info$var_name
  col_types <- do.call(readr::cols_only, col_types)
}

rectype_label_names <- function(cur_names, ddi) {
  # If value labels for rectype are available use them to name data.frames
  rt_lbls <- ddi$var_info$val_labels[[which(ddi$var_info$var_name == ddi$rectype_idvar)]]
  matched_lbls <- match(cur_names, rt_lbls$val)
  # If any don't match, don't rename for fear of making thing worse
  if (any(is.na(matched_lbls))) {
    return(cur_names)
  }

  rt_lbls <- rt_lbls$lbl[matched_lbls]
  # Clean up value labels a bit though:
  rt_lbls <- toupper(rt_lbls)
  rt_lbls <- stringr::str_replace_all(rt_lbls, " RECORD$", "")
  rt_lbls <- stringr::str_replace_all(rt_lbls, "[:blank:]", "_")

  rt_lbls
}
