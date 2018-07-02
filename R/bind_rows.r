# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

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

  purrr::iwalk(purrr::keep(attrs_by_var, ~!is_FALSE(.)), function(attr, vname) {
    for (iii in seq_along(d_list)) {
      if (vname %in% names(d_list[[iii]])) {
        d_list[[iii]][[vname]] <<- zap_ipums_attributes(d_list[[iii]][[vname]])
      }
    }
  })

  out <- dplyr::bind_rows(d_list, .id = .id)

  purrr::iwalk(purrr::keep(attrs_by_var, ~!is_FALSE(.)), function(attr, vname) {
    attributes(out[[vname]]) <<- attr
  })
  out
}

is_FALSE <- function(x) identical(x, FALSE)
