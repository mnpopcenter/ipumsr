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

  ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

  if (!is.null(callback$set_ipums_fields)) {
    if (is.null(ddi$rectype_idvar)) {
      rec_vinfo <- NULL
    } else {
      rec_vinfo <- dplyr::filter(ddi$var_info, .data$var_name == ddi$rectype_idvar)
    }
    callback$set_ipums_fields("long", ddi, var_attrs, ddi)
  }

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") stop("Hierarchical data cannot be read as csv.")
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv_chunked(
      data_file,
      callback,
      chunk_size,
      col_types = col_types,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
  } else {
    rt_info <- ddi_to_rtinfo(ddi)
    col_spec <- ddi_to_colspec(ddi, "long", verbose)
    out <- hipread::hipread_long_chunked(
      data_file,
      callback,
      chunk_size,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      encoding = ddi$file_encoding
    )
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

  # rectype can be removed from ddi, so keep it for use later
  rt_ddi <- get_rt_ddi(ddi)
  ddi <- ddi_filter_vars(ddi, vars, "list", verbose)

  if (!is.null(callback$set_ipums_fields)) {
    callback$set_ipums_fields("list", ddi, var_attrs, rt_ddi)
  }

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") stop("Hierarchical data cannot be read as csv.")
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv_chunked(
      data_file,
      callback,
      chunk_size,
      col_types = col_types,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
    if (verbose) cat("Assuming data rectangularized to 'P' record type")
    out <- list("P" = out)
  } else {
    rt_info <- ddi_to_rtinfo(rt_ddi)
    col_spec <- ddi_to_colspec(ddi, "list", verbose)
    out <- hipread::hipread_list_chunked(
      data_file,
      callback,
      chunk_size,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      encoding = ddi$file_encoding
    )
  }

  out
}

ipumsify_data <- function(
  data, data_structure, ddi, var_attrs, rt_ddi
) {
  if (data_structure == "long") {
    out <- set_ipums_var_attributes(data, ddi$var_info, var_attrs)
  } else if (data_structure == "list") {
    out <- data
    for (rt in names(out)) {
      out[[rt]] <- set_ipums_var_attributes(out[[rt]], ddi, var_attrs)
    }
    names(out) <- rectype_label_names(names(out), rt_ddi)
  } else {
    stop("Don't know what to do with data structure: ", data_structure)
  }
  out
}
