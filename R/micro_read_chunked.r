# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr


#' Read data from an IPUMS extract (in chunks)
#'
#' Reads a dataset downloaded from the IPUMS extract system, but does
#' so by reading a chunk, then applying your code to that chunk and
#' then continuing, which can allow you to deal with data that is
#' too large to store in your computer's RAM all at once.
#'
#' @inheritParams read_ipums_micro
#' @param callback An \code{\link{ipums_callback}} object, or a function
#'   that will be converted to an IpumsSideEffectCallback object.
#' @param chunk_size An integer indicating how many observations to
#'   read in per chunk (defaults to 10,000). Setting this higher
#'   uses more RAM, but will usually be faster.
#'
#' @return Depends on the callback object
#' @export
#' @family ipums_read
#' @examples
#' # Select Minnesotan cases from CPS example (Note you can also accomplish
#' # this and avoid having to even download a huge file using the "Select Cases"
#' # functionality of the IPUMS extract system)
#' mn_only <- read_ipums_micro_chunked(
#'   ipums_example("cps_00006.xml"),
#'   IpumsDataFrameCallback$new(function(x, pos) {
#'     x[x$STATEFIP == 27, ]
#'   }),
#'   chunk_size = 1000 # Generally you want this larger, but this example is a small file
#' )
#'
#' # Tabulate INCTOT average by state without storing full dataset in memory
#' library(dplyr)
#' inc_by_state <- read_ipums_micro_chunked(
#'   ipums_example("cps_00006.xml"),
#'   IpumsDataFrameCallback$new(function(x, pos) {
#'     x %>%
#'       mutate(
#'         INCTOT = lbl_na_if(
#'           INCTOT, ~.lbl %in% c("Missing.", "N.I.U. (Not in Universe)."))
#'         ) %>%
#'       filter(!is.na(INCTOT)) %>%
#'       group_by(STATEFIP = as_factor(STATEFIP)) %>%
#'       summarize(INCTOT_SUM = sum(INCTOT), n = n())
#'   }),
#'   chunk_size = 1000 # Generally you want this larger, but this example is a small file
#' ) %>%
#' group_by(STATEFIP) %>%
#' summarize(avg_inc = sum(INCTOT_SUM) / sum(n))
#'
#' # x will be a list when using `read_ipums_micro_list_chunked()`
#' read_ipums_micro_list_chunked(
#'   ipums_example("cps_00010.xml"),
#'   IpumsSideEffectCallback$new(function(x, pos) {
#'     print(paste0(nrow(x$PERSON), " persons and ", nrow(x$HOUSEHOLD), " households in this chunk."))
#'   }),
#'   chunk_size = 1000 # Generally you want this larger, but this example is a small file
#' )
#'
#' # Using the biglm package, you can even run a regression without storing
#' # the full dataset in memory
#' library(dplyr)
#' if (require(biglm)) {
#'   lm_results <- read_ipums_micro_chunked(
#'     ipums_example("cps_00015.xml"),
#'     IpumsBiglmCallback$new(
#'       INCTOT ~ AGE + HEALTH, # Simple regression (may not be very useful)
#'       function(x, pos) {
#'         x %>%
#'         mutate(
#'           INCTOT = lbl_na_if(
#'             INCTOT, ~.lbl %in% c("Missing.", "N.I.U. (Not in Universe).")
#'           ),
#'           HEALTH = as_factor(HEALTH)
#'         )
#'     }),
#'     chunk_size = 1000 # Generally you want this larger, but this example is a small file
#'   )
#'   summary(lm_results)
#' }
#'
read_ipums_micro_chunked <- function(
  ddi,
  callback,
  chunk_size = 10000,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  rectype_convert = NULL,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) custom_cat(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  if (is.function(callback)) callback <- IpumsSideEffectCallback$new(callback)

  if (ddi$file_type == "hierarchical") {
    out <- read_ipums_hier_chunked(
      ddi, callback, chunk_size, vars, "long", data_file, verbose,
      rectype_convert, var_attrs
    )
  } else if (ddi$file_type == "rectangular") {
    out <- read_ipums_rect_chunked(
      ddi, callback, chunk_size, vars, data_file, verbose, var_attrs
    )
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

  out
}

#' @export
#' @rdname read_ipums_micro_chunked
read_ipums_micro_list_chunked <- function(
  ddi,
  callback,
  chunk_size = 10000,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  rectype_convert = NULL,
  var_attrs = c("val_labels", "var_label", "var_desc")
) {
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) custom_cat(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)
  if (is.function(callback)) callback <- IpumsSideEffectCallback$new(callback)

  if (ddi$file_type == "hierarchical") {
    out <- read_ipums_hier_chunked(
      ddi, callback, chunk_size, vars, "list", data_file, verbose,
      rectype_convert, var_attrs
    )
  } else if (ddi$file_type == "rectangular") {
    out <- read_ipums_rect_chunked(
      ddi, callback, chunk_size, vars, data_file, verbose, var_attrs
    )
    warning("Assuming data rectangularized to 'P' record type")
    out <- list(P = out)
  } else {
    stop(paste0("Don't know how to read ", ddi$file_type, " type file."), call. = FALSE)
  }

  out
}

read_ipums_hier_chunked <- function(
  ddi, callback, chunk_size, vars, data_structure, data_file,
  verbose, rectype_convert, var_attrs
) {
  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    stop("Hierarchical data cannot be read as csv.")
  }
  all_vars <- ddi$var_info

  rec_vinfo <- dplyr::filter(all_vars, .data$var_name == ddi$rectype_idvar)
  if (nrow(rec_vinfo) > 1) stop("Cannot support multiple rectype id variables.", call. = FALSE)
  hip_rec_vinfo <- hipread::hip_rt(rec_vinfo$start, rec_vinfo$end - rec_vinfo$start + 1)

  all_vars <- select_var_rows(all_vars, vars)
  if (!rec_vinfo$var_name %in% all_vars$var_name && data_structure == "long") {
    if (verbose) {
      cat(paste0("Adding rectype id var '", rec_vinfo$var_name, "' to data.\n\n"))
    }
    all_vars <- dplyr::bind_rows(rec_vinfo, all_vars)
  }

  if (data_structure == "list") {
    key_vars <- purrr::flatten_chr(ddi$rectypes_keyvars$keyvars)
    missing_kv <- dplyr::setdiff(key_vars, all_vars$var_name)
    if (length(missing_kv) > 0) {
      kv_rows <- select_var_rows(ddi$var_info, rlang::as_quosure(missing_kv))

      if (verbose) {
        cat(paste0(
          "Adding cross rectype linking vars ('",
          paste(missing_kv, collapse = "', '"),
          "') to data.\n\n"
        ))
      }
      all_vars <- dplyr::bind_rows(kv_rows, all_vars)
    }
  }

  col_info <- tidyr::unnest_(all_vars, "rectypes", .drop = FALSE)
  rts <- unique(col_info$rectypes)
  col_info <- purrr::map(rts, function(rt) {
    rt_cinfo <- col_info[col_info$rectypes == rt, ]
    hipread::hip_fwf_positions(
      rt_cinfo$start,
      rt_cinfo$end,
      rt_cinfo$var_name,
      hipread_type_name_convert(rt_cinfo$var_type)
    )
  })
  names(col_info) <- rts

  if (!is.null(callback$set_ipums_fields)) {
    callback$set_ipums_fields(data_structure, all_vars, var_attrs, rec_vinfo)
  }


  if (data_structure == "long") {
    out <- hipread::hipread_long_chunked(
      data_file,
      callback,
      chunk_size,
      col_info,
      hip_rec_vinfo,
      progress = show_readr_progress(verbose)
    )
  } else if (data_structure == "list") {
    out <- hipread::hipread_list_chunked(
      data_file,
      callback,
      chunk_size,
      col_info,
      hip_rec_vinfo,
      progress = show_readr_progress(verbose)
    )

    out
  }
}

read_ipums_rect_chunked <- function(
  ddi, callback, chunk_size, vars, data_file, verbose, var_attrs
) {
  all_vars <- select_var_rows(ddi$var_info, vars)

  col_types <- purrr::map(all_vars$var_type, function(x) {
    if (x == "numeric") out <- readr::col_double()
    else if(x == "character") out <- readr::col_character()
    else if (x == "integer") out <- readr::col_integer()
    out
  })
  names(col_types) <- all_vars$var_name
  col_types <- do.call(readr::cols_only, col_types)

  col_positions <- readr::fwf_positions(
    start = all_vars$start,
    end = all_vars$end,
    col_names = all_vars$var_name
  )

  is_fwf <- ipums_file_ext(data_file) %in% c(".dat", ".dat.gz")
  is_csv <- ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")

  if (!is.null(callback$set_ipums_fields)) {
    callback$set_ipums_fields("long", all_vars, var_attrs)
  }

  if (is_fwf) {
    out <- hipread::hipread_long_chunked(
      data_file,
      callback,
      chunk_size,
      readr_to_hipread_specs(col_positions, col_types),
      encoding = ddi$file_encoding,
      progress = show_readr_progress(verbose)
    )
  } else if (is_csv) {
    out <- read_check_for_negative_bug(
      readr::read_csv_chunked,
      data_file,
      callback,
      chunk_size,
      col_types = col_types,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
  } else {
    stop("Unrecognized file type.")
  }

  out
}

ipumsify_data <- function(
  data, data_structure, all_vars, var_attrs, rec_vinfo
) {
  if (data_structure == "long") {
    out <- set_ipums_var_attributes(data, all_vars, var_attrs)
    out <- set_imp_decim(out, all_vars)
  } else if (data_structure == "list") {
    out <- data
    for (rt in names(out)) {
      rt_vinfo <- all_vars[purrr::map_lgl(all_vars$rectypes, ~rt %in% .), ]
      out[[rt]] <- set_ipums_var_attributes(out[[rt]], rt_vinfo, var_attrs)
      out[[rt]] <- set_imp_decim(out[[rt]], rt_vinfo)
    }
    # If value labels for rectype are available use them to name data.frames
    rt_lbls <- rec_vinfo$val_labels[[1]]
    matched_lbls <- match(names(out), rt_lbls$val)
    if (all(!is.na(matched_lbls))) {
      # Can use the value labels
      rt_lbls <- rt_lbls$lbl[matched_lbls]
      # Clean it up a bit though: all upper case
      rt_lbls <- toupper(rt_lbls)
      # drop trailing 'record'
      rt_lbls <- stringr::str_replace_all(rt_lbls, " RECORD$", "")
      # and replace blank space with _
      rt_lbls <- stringr::str_replace_all(rt_lbls, "[:blank:]", "_")
      names(out) <- rt_lbls
    }
  } else {
    stop("Don't know what to do with data structure: ", data_structure)
  }
  out
}
