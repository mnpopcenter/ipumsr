# This file is part of the Minnesota Population Center's ipumsr.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsr

#' @export
print.ipums_ddi <- function(x, ...) {
  fproject <- ifelsenull(x$ipums_project, "<missing project>")
  fvarnum <- ifelsenull(x$var_info, "<???>", nrow)
  fname <- ifelsenull(x$file_name, "<missing file name>")
  fdate <- ifelsenull(x$extract_date, "<missing date>")
  fnotes <- ifelsenull(x$extract_notes, "No user notes found.")

  custom_cat(
    "An IPUMS DDI for ", fproject, " with ", fvarnum, " variables\n",
    "Extract '", fname, "' created on ", fdate, "\n",
    "User notes:\n"
  )
  custom_cat(fnotes, indent = 2, exdent = 2)
}


ifelsenull <- function(x, y, .f = identity) {
  if (is.null(x)) return(y)
  .f(x)
}
