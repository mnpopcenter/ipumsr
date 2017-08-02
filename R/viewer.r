# This file is part of the Minnesota Population Center's ipumsimport.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ipumsimport

#' View a static webpage with variable information from a IPUMS extract
#'
#' Requires that htmltools, shiny and DT are installed.
#'
#'@param var_info Variable information from a IPUMS extract (from \code{\link{ipums_var_info}})
#'@param out_file Optionally specify a location to save HTML file. NULL the default
#'  makes a temporary file.
#'@export
ipums_view <- function(var_info, out_file = NULL) {
  if (
    !requireNamespace("htmltools", quietly = TRUE) ||
    !requireNamespace("shiny", quietly = TRUE) ||
    !requireNamespace("DT", quietly = TRUE)
  ) {
    stop(paste0(
      "Please install htmltools, shiny, and DT using ",
      "`install.packages(c('htmltools', 'shiny', 'DT')"
    ))
  }
  if (is.null(out_file)) out_file <- paste0(tempfile(), ".html")

  htmltools::save_html(shiny::basicPage(
    htmltools::tags$h1("IPUMS Data Dictionary Viewer"),
    purrr::pmap(var_info, display_ipums_var_row)
  ), out_file)


  if (requireNamespace("rstudioapi")) {
    rstudioapi::viewer(out_file)
  } else {
    cat(paste0("See HTML file at: ", out_file))
  }
}


display_ipums_var_row <- function(var_name, var_label, var_desc, val_labels, ...) {
  vd_html <- split_double_linebreaks_to_ptags(var_desc)

  if (nrow(val_labels) > 0) {
    value_labels <- DT::datatable(val_labels, width = "100%")
  } else {
    value_labels <- htmltools::tags$p("N/A")
  }



  expandable_div(
    var_name,
    var_label,
    shiny::fluidRow(
      shiny::column(6, htmltools::tags$h4("Variable Description"), vd_html),
      shiny::column(6, htmltools::tags$h4("Value Labels"), value_labels)
    )
  )
}


expandable_div <- function(title, subtitle, content) {
  htmltools::tags$div(
    class = "panel panel-default",
    htmltools::tags$div(
      class = "panel-heading",
      htmltools::tags$div(
        class = "panel-title",
        htmltools::tags$a(
          class = "accordion-toggle",
          `data-toggle` = "collapse",
          `data-parent` = "#accordion",
          `aria-expanded` = "false",
          href = paste0("#", title),
          htmltools::tags$div(
            htmltools::tags$i(class = "more-less glyphicon glyphicon-plus"),
            htmltools::tags$h3(
              title,
              style = "display:inline-block; padding-right:0.5em"
            ),
            htmltools::tags$h5(subtitle)
          )
        )
      )
    ),
    htmltools::tags$div(
      id = title,
      class = "panel-colapse collapse",
      htmltools::tags$div(
        class = "panel-body",
        content
      )
    )
  )
}

split_double_linebreaks_to_ptags <- function(x) {
  out <- stringr::str_split(x, "\n\n")[[1]]
  purrr::map(out, htmltools::tags$p)
}
