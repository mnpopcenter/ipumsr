# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr


#' Read data from an IPUMS extract
#'
#' Reads a dataset downloaded from the IPUMS extract system.
#' For IPUMS projects with microdata, it relies on a downloaded
#' DDI codebook and a fixed-width file. Loads the data with
#' value labels (using \code{\link[haven]{labelled}} format)
#' and variable labels. See 'Details' for more information on
#' how record types are handled by the ipumsr package.
#'
#' Some IPUMS projects have data for multiple types of records
#' (eg Household and Person). When downloading data from many of these
#' projects you have the option for the IPUMS extract system
#' to "rectangularize" the data, meaning that the data is
#' transformed so that each row of data represents only one
#' type of record.
#'
#' There also is the option to download "hierarchical" extracts,
#' which are a single file with record types mixed in the rows.
#' The ipumsr package offers two methods for importing this data.
#'
#' \code{read_ipums_micro} loads this data into a "long" format
#' where the record types are mixed in the rows, but the variables
#' are \code{NA} for the record types that they do not apply to.
#'
#' \code{read_ipums_micro_list} loads the data into a list of
#' data frames objects, where each data frame contains only
#' one record type. The names of the data frames in the list
#' are the text from the record type labels without 'Record'
#' (often 'HOUSEHOLD' for Household and 'PERSON' for Person).
#'
#' @param ddi Either a filepath to a DDI xml file downloaded from
#'   the website, or a \code{ipums_ddi} object parsed by \code{\link{read_ipums_ddi}}
#' @param vars Names of variables to load. Accepts a character vector of names, or
#'  \code{\link{dplyr_select_style}} conventions. For hierarchical data, the
#'  rectype id variable will be added even if it is not specified.
#' @param n_max The maximum number of records to load.
#' @param data_file Specify a directory to look for the data file.
#'   If left empty, it will look in the same directory as the DDI file.
#' @param verbose Logical, indicating whether to print progress information
#'   to console.
#' @param var_attrs Variable attributes to add from the DDI, defaults to
#'   adding all (val_labels, var_label and var_desc). See
#'   \code{\link{set_ipums_var_attributes}} for more details.
#' @param lower_vars Only if reading a DDI from a file, a logical indicating
#'   whether to convert variable names to lowercase (default is FALSE, in line
#'   with IPUMS conventions). Note that this argument will be ignored if
#'   argument \code{ddi} is an \code{ipums_ddi} object rather than a file path.
#'   See \code{\link{read_ipums_ddi}} for converting variable names to lowercase
#'   when reading in the DDI.
#' @return \code{read_ipums_micro} returns a single tbl_df data frame, and
#'   \code{read_ipums_micro_list} returns a list of data frames, named by
#'   the Record Type. See 'Details' for more
#'   information.
#' @examples
#'   # Rectangular example file
#'   cps_rect_ddi_file <- ipums_example("cps_00006.xml")
#'
#'   cps <- read_ipums_micro(cps_rect_ddi_file)
#'   # Or load DDI separately to keep the metadata
#'   ddi <- read_ipums_ddi(cps_rect_ddi_file)
#'   cps <- read_ipums_micro(ddi)
#'
#'   # Hierarchical example file
#'   cps_hier_ddi_file <- ipums_example("cps_00010.xml")
#'
#'   # Read in "long" format and you get 1 data frame
#'   cps_long <- read_ipums_micro(cps_hier_ddi_file)
#'   head(cps_long)
#'
#'   # Read in "list" format and you get a list of multiple data frames
#'   cps_list <- read_ipums_micro_list(cps_hier_ddi_file)
#'   head(cps_list$PERSON)
#'   head(cps_list$HOUSEHOLD)
#'
#'   # Or you can use the \code{%<-%} operator from zeallot to unpack
#'   c(household, person) %<-% read_ipums_micro_list(cps_hier_ddi_file)
#'   head(person)
#'   head(household)
#'
#' @family ipums_read
#' @export
read_ipums_micro <- function(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    warning(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) custom_cat(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") stop("Hierarchical data cannot be read as csv.")
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv(
      data_file,
      col_types = col_types,
      n_max = n_max,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
    if (ddi_has_lowercase_var_names(ddi)) {
      out <- dplyr::rename_all(out, tolower)
    }
  } else {
    rt_info <- ddi_to_rtinfo(ddi)
    col_spec <- ddi_to_colspec(ddi, "long", verbose)
    out <- hipread::hipread_long(
      data_file,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      n_max = n_max,
      encoding = ddi$file_encoding
    )
  }

  out <- set_ipums_var_attributes(out, ddi, var_attrs)

  out
}

#' @export
#' @rdname read_ipums_micro
read_ipums_micro_list <- function(
  ddi,
  vars = NULL,
  n_max = Inf,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  lower_vars_was_ignored <- check_if_lower_vars_ignored(ddi, lower_vars)
  if (lower_vars_was_ignored) {
    warning(lower_vars_ignored_warning())
  }
  if (is.character(ddi)) ddi <- read_ipums_ddi(ddi, lower_vars = lower_vars)
  if (is.null(data_file)) data_file <- file.path(ddi$file_path, ddi$file_name)

  data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

  if (verbose) custom_cat(short_conditions_text(ddi))

  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  # rectype can be removed from ddi, so keep it for use later
  rt_ddi <- get_rt_ddi(ddi)
  ddi <- ddi_filter_vars(ddi, vars, "list", verbose)

  if (ipums_file_ext(data_file) %in% c(".csv", ".csv.gz")) {
    if (ddi$file_type == "hierarchical") stop("Hierarchical data cannot be read as csv.")
    col_types <- ddi_to_readr_colspec(ddi)
    out <- readr::read_csv(
      data_file,
      col_types = col_types,
      n_max = n_max,
      locale = ipums_locale(ddi$file_encoding),
      progress = show_readr_progress(verbose)
    )
    if (ddi_has_lowercase_var_names(ddi)) {
      out <- dplyr::rename_all(out, tolower)
    }
    if (verbose) cat("Assuming data rectangularized to 'P' record type")
    out <- list("P" = out)
  } else {
    rt_info <- ddi_to_rtinfo(rt_ddi)
    col_spec <- ddi_to_colspec(ddi, "list", verbose)
    out <- hipread::hipread_list(
      data_file,
      col_spec,
      rt_info,
      progress = show_readr_progress(verbose),
      n_max = n_max,
      encoding = ddi$file_encoding
    )
    names(out) <- rectype_label_names(names(out), rt_ddi)
  }

  for (rt in names(out)) {
    out[[rt]] <- set_ipums_var_attributes(out[[rt]], ddi, var_attrs)
  }

  out
}

#' Warns the user that lower_vars has been ignored when they supply an ipums_ddi
#' to a data reading function
#' @noRd
check_if_lower_vars_ignored <- function(ddi, lower_vars) {
  inherits(ddi, "ipums_ddi") & lower_vars
}

lower_vars_ignored_warning <- function() {
  paste0(
    "Argument lower_vars was set to TRUE but has been ignored because ",
    "argument ddi is an ipums_ddi object. To obtain lowercase names in both ",
    "the ipums_ddi object and the data, set lower_vars to TRUE in your call ",
    "to function `read_ipums_ddi`."
  )
}
