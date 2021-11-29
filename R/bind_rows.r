# This file is part of the ipumsr R package created by IPUMS.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/ipums/ipumsr

#' Bind rows together, but preserve labelled class attributes
#'
#' @param ... Either data.frames or list of data.frames
#' @param .id Data frame identifier, when arguments are named (or are named lists
#'   of data.frames), will make a new column with this name that has the original
#'   names.
#'
#' @return A data.frame
#' @export
ipums_bind_rows <- function(..., .id = NULL) {
  # TODO: Rewrite in C++?
  # Definitely not exactly the same logic as dplyr, but should cover most cases
  d_list <- rlang::squash_if(
    rlang::dots_values(...),
    function(x) is.list(x) && !is.data.frame(x)
  )

  unique_var_names <- unique(purrr::flatten_chr(purrr::map(d_list, names)))

  attrs_by_var <- purrr::map(unique_var_names, function(vvv) {
    all_attrs <- purrr::map(purrr::keep(d_list, ~vvv %in% names(.)), ~attributes(.[[vvv]]))
    if (length(all_attrs) == 1) return(all_attrs[[1]])

    first_attrs <- all_attrs[[1]]
    all_equal <- purrr::map_lgl(all_attrs[-1], ~identical(., first_attrs))
    if (all(all_equal)) return(first_attrs) else return(FALSE)
  })
  names(attrs_by_var) <- unique_var_names

  # Remove all IPUMS attributes (we will put back the matching ones after
  # row-binding)
  d_list <- purrr::map(d_list, zap_ipums_attributes)

  vars_w_incompat_attrs <- unique_var_names[purrr::map_lgl(attrs_by_var, ~ is_FALSE(.))]
  if (length(vars_w_incompat_attrs) > 0) {
    warning(
      "IPUMS attributes have been removed from the following columns, which ",
      "had incompatible attributes across the data.frames to be combined: ",
      paste0(vars_w_incompat_attrs, collapse = ",")
    )
  }

  out <- dplyr::bind_rows(d_list, .id = .id)

  # Reassign attributes for columns where all attributes matched
  purrr::iwalk(purrr::keep(attrs_by_var, ~!is_FALSE(.)), function(attr, vname) {
    attributes(out[[vname]]) <<- attr
  })
  out
}

is_FALSE <- function(x) identical(x, FALSE)
