#' Read a dataset from the output folder using the YAML
#'
#' Reads a dataset from the output folder for analysis. Not as fast as
#' quick.freq.pl and other unix tools, but if you select only a few
#' variables, can go pretty fast (even on your local computer).
#' @export
#' @param yaml Either a character indicating a filepath to the YAML, or
#'             a dataset from a YAML already loaded using \code{\link{read_var_info_yaml}}.
#' @param data_file Filepath to fixed-width file.
#' @param vars Dplyr \code{\link[dplyr]{select}}-style group of variables to be loaded.
#' @param n_max An integer which specifies the maximum number of rows to read
#'              from the dataset.
#' @examples
#' \dontrun{
#' data <- read_ipums_output("ses2013c_sestat.yml",
#'                           "ses2013c_highered.dat"
#'                           c(RECTYPE, PERSONID, ND2MED, ND2NED),
#'                           )
#' }
read_ipums_output <- function(yaml, data_file, vars = everything(),
                                  n_max = Inf) {

  if (inherits(yaml, "character")) {
    var_info <- read_var_info_yaml(yaml)
  } else {
    var_info <- yaml
  }
  if (is.null(data_file)) data_file <- attr(var_info, "data_file")

  # Make columns that are easier to pass into readr
  var_info <- dplyr::mutate_(var_info, end_column = ~start_column + width - 1)

  vars <- lazyeval::lazy(vars)
  if (!is.null(vars)) {
    vars <- dplyr::select_vars_(unique(var_info$name), args = vars)
    var_info <- var_info[var_info$name %in% vars, ]
  }

  out <- readr::read_fwf(data_file,
                         readr::fwf_positions(var_info$start_column, var_info$end_column,
                                              var_info$name),
                         paste(rep("c", nrow(var_info)), collapse = ""),
                         n_max = n_max, progress = FALSE)


  # Convert to numeric (if statement because of dplyr#1989)
  if (any(!var_info$is_string_var)) {
    out <- dplyr::mutate_if(out, !var_info$is_string_var, as_numeric)
  }

  # Variable info specific fixes
  out <- make_imp_decimals(out, var_info$name, var_info$implied_decimals)
  out <- add_value_labels(out, var_info$name, var_info$values)
  out <- add_variable_labels(out, var_info$name, var_info$label)

  out
}


#' Read a the YAML metadata on variables from an output folder.
#'
#' Reads in the YAML metadata. Though usually called from within
#' \code{\link{read_ipums_output}}, this function can be useful
#' for learning about the variables before loading the full dataset.
#' @export
#' @param file filepath to a YAML (.yml) file in the output folder.
read_var_info_yaml <- function(file) {
  d_yaml <- yaml::yaml.load_file(file)

  v_info <- purrr::map_df(d_yaml$variables, ~.[c("name", "label", "start_column", "width", "is_string_var",
                                                 "is_common_var", "record_type", "implied_decimals")])
  # Nested data.frames hold the information about value encodings
  v_info$values <- purrr::map(d_yaml$variables, ~dplyr::bind_rows(.$values))

  v_info
}


# Code complicated enough to pull out - divides by 10^implied decimals number
make_imp_decimals <- function(data, vname, imp_dec) {
  vname <- vname[imp_dec > 0 && !is.na(imp_dec)]
  imp_dec <- imp_dec[imp_dec > 0 && !is.na(imp_dec)]


  # loops through the implied decimals columns and makes a mutate statement for them
  imp_dec_f <- lapply(seq_len(length(imp_dec)), function(x) {
    lazyeval::interp(~vvv / 10L ^ lll, .values = list(vvv = as.name(vname[x]), lll = imp_dec[x]))
  })

  dplyr::mutate_(data, .dots = purrr::set_names(imp_dec_f, vname))
}


as_numeric <- function(x) {
  qn <- purrr::quietly(as.numeric)
  out <- qn(x)
  if (length(out$warnings > 0)) {
    attr(out$result, "unexpected_values") <- unique(x[is.na(out$result) & !is.na(x)])
    warning("Non-numeric values coersed to NA", call. = FALSE)
  }
  out$result
}
