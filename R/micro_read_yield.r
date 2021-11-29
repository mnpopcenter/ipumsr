# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Read data from an IPUMS extract (in yields)
#'
#' Reads a dataset downloaded from the IPUMS extract system, but does
#' so by returning an object that can read a group of lines at a time.
#' This is a more flexible way to read data in chunks than
#' the functions like \code{\link{read_ipums_micro_chunked}}, allowing
#' you to do things like reading parts of multiple files at the same time
#' and resetting from the beginning more easily than with the chunked
#' functions. \strong{Note that while other \code{read_ipums_micro*} functions
#' can read from .csv(.gz) or .dat(.gz) files, these functions can only read
#' from .dat(.gz) files.}
#'
#' These functions return an IpumsYield R6 object which have the following
#' methods:
#' \itemize{
#' \item \code{yield(n = 10000)} A function to read the next 'yield' from the data,
#'   returns a `tbl_df` (or list of `tbl_df` for `hipread_list_yield()`)
#'   with up to n rows (it will return NULL if no rows are left, or all
#'   available ones if less than n are available).
#' \item \code{reset()} A function to reset the data so that the next yield will
#'   read data from the start.
#' \item \code{is_done()} A function that returns whether the file has been completely
#'   read yet or not.
#' \item \code{cur_pos} A property that contains the next row number that will be
#'    read (1-indexed).
#' }
#' @inheritParams read_ipums_micro
#'
#' @return A HipYield R6 object (See 'Details' for more information)
#' @export
#' @family ipums_read
#' @examples
#' # An example using "long" data
#' long_yield <- read_ipums_micro_yield(ipums_example("cps_00006.xml"))
#' # Get first 10 rows
#' long_yield$yield(10)
#' # Get 20 more rows now
#' long_yield$yield(20)
#' # See what row we're on now
#' long_yield$cur_pos
#' # Reset to beginning
#' long_yield$reset()
#' # Read the whole thing in chunks and count Minnesotans
#' total_mn <- 0
#' while (!long_yield$is_done()) {
#'   cur_data <- long_yield$yield(1000)
#'   total_mn <- total_mn + sum(as_factor(cur_data$STATEFIP) == "Minnesota")
#' }
#' total_mn
#'
#' # Can also read hierarchical data as list:
#' list_yield <- read_ipums_micro_list_yield(ipums_example("cps_00006.xml"))
#' list_yield$yield(10)
#'
read_ipums_micro_yield <- function(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  IpumsLongYield$new(
    ddi = ddi,
    vars = !!vars,
    data_file = data_file,
    verbose = verbose,
    var_attrs = var_attrs,
    lower_vars = lower_vars
  )
}

#' @export
#' @rdname read_ipums_micro_yield
read_ipums_micro_list_yield <- function(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
) {
  vars <- enquo(vars)
  if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

  IpumsListYield$new(
    ddi = ddi,
    vars = !!vars,
    data_file = data_file,
    verbose = verbose,
    var_attrs = var_attrs,
    lower_vars = lower_vars
  )
}


#' @export
#' @rdname read_ipums_micro_yield
IpumsLongYield <- R6::R6Class(
  "IpumsLongYield", inherit = hipread::HipLongYield,
  cloneable = FALSE,
  private = list(ddi = NULL, var_attrs = NULL),
  public = list(
    initialize = function(
      ddi,
      vars = NULL,
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
      if (fostr_detect(data_file, "\\.csv$|\\.csv\\.gz$")) {
        stop(
          "read_ipums_micro_yield does not support reading from .csv ",
          "formatted data files, only from .dat (or .dat.gz) files"
        )
      }

      data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

      if (verbose) custom_cat(short_conditions_text(ddi))

      vars <- enquo(vars)
      if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

      ddi <- ddi_filter_vars(ddi, vars, "long", verbose)

      rt_info <- ddi_to_rtinfo(ddi)
      col_spec <- ddi_to_colspec(ddi, "long", verbose)

      super$initialize(
        file = data_file,
        var_info = col_spec,
        rt_info = rt_info,
        encoding = ddi$encoding
      )

      private$ddi <- ddi
      private$var_attrs <- var_attrs
    },
    yield = function(n = 10000) {
      out <- super$yield(n = n)
      if (is.null(out)) return(out)
      out <- set_ipums_var_attributes(out, private$ddi, private$var_attrs)
      out
    }
  )
)

#' @export
#' @rdname read_ipums_micro_yield
IpumsListYield <- R6::R6Class(
  "IpumsListYield", inherit = hipread::HipListYield,
  cloneable = FALSE,
  private = list(ddi = NULL, var_attrs = NULL, rt_ddi = NULL),
  public = list(
    initialize = function(
      ddi,
      vars = NULL,
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
      if (fostr_detect(data_file, "\\.csv$|\\.csv\\.gz$")) {
        stop(
          "read_ipums_micro_yield does not support reading from .csv ",
          "formatted data files, only from .dat (or .dat.gz) files"
        )
      }

      data_file <- custom_check_file_exists(data_file, c(".dat.gz", ".csv", ".csv.gz"))

      if (verbose) custom_cat(short_conditions_text(ddi))

      vars <- enquo(vars)
      if (!is.null(var_attrs)) var_attrs <- match.arg(var_attrs, several.ok = TRUE)

      # rectype can be removed from ddi, so keep it for use later
      rt_ddi <- get_rt_ddi(ddi)
      ddi <- ddi_filter_vars(ddi, vars, "list", verbose)

      rt_info <- ddi_to_rtinfo(ddi)
      col_spec <- ddi_to_colspec(ddi, "list", verbose)

      super$initialize(
        file = data_file,
        var_info = col_spec,
        rt_info = rt_info,
        encoding = ddi$encoding
      )

      private$ddi <- ddi
      private$var_attrs <- var_attrs
      private$rt_ddi <- rt_ddi
    },
    yield = function(n = 10000) {
      out <- super$yield(n = n)
      if (is.null(out)) return(out)
      names(out) <- rectype_label_names(names(out), private$rt_ddi)
      for (rt in names(out)) {
        out[[rt]] <- set_ipums_var_attributes(out[[rt]], private$ddi, private$var_attrs)
      }
      out
    }
  )
)
