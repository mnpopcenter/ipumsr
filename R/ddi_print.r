# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' @export
print.ipums_ddi <- function(x, ...) {
  fproject <- ifelsenull(x$ipums_project, "<missing project>")
  fvarnum <- ifelsenull(x$var_info, "<???>", nrow)
  fname <- ifelsenull(x$file_name, "<missing file name>")
  fdate <- ifelsenull(x$extract_date, "<missing date>")
  fnotes <- ifelsenull(x$extract_notes, "No user notes found.")

  cat(paste0("An IPUMS DDI for ", fproject, " with ", fvarnum, " variables\n"))
  cat(paste0("Extract '", fname, "' created on ", fdate, "\n"))
  cat(paste0("User notes:\n", fnotes, "\n"))
}


ifelsenull <- function(x, y, .f = identity) {
  if (is.null(x)) return(y)
  .f(x)
}
