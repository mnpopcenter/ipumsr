# This file is part of the Minnesota Population Center's ripums.
# For copyright and licensing information, see the NOTICE and LICENSE files
# in this project's top-level directory, and also on-line at:
#   https://github.com/mnpopcenter/ripums

#' View a static webpage with variable information from a IPUMS extract
#'
#' Requires that htmltools, shiny and DT are installed.
#'
#'@param x A DDI or other object with ipums attributes (such as data loaded from an extract)
#'@param out_file Optionally specify a location to save HTML file. NULL the default
#'  makes a temporary file.
#'@export
ipums_view <- function(x, out_file = NULL) {
  UseMethod("ipums_view")
}

#' @export
ipums_view.default <- function(x, out_file = NULL) {
  ipums_view_base(ipums_var_info(x), out_file)
}

ipums_view_base <- function(var_info, out_file = NULL) {
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



  html_page <- shiny::basicPage(
    htmltools::tags$h1("IPUMS Data Dictionary Viewer"),
    purrr::pmap(var_info, display_ipums_var_row)
  )

  html_page <- add_jquery_dependency(html_page)


  htmltools::save_html(html_page, out_file)


  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudioapi::viewer(out_file)
  } else {
    cat(paste0("See HTML file at: ", out_file))
  }
}


display_ipums_var_row <- function(var_name, var_label, var_desc, val_labels, ...) {
  if (is.na(var_label)) var_label <- "-"

  if (is.na(var_desc)) var_desc <- "No variable description available..."
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


add_jquery_dependency <- function(page) {
  # Get jquery file from DT package's installed files (in case
  # no DT's are included in output)
  jquery_dir <- c(
    href = "shared/jquery",
    file = system.file("htmlwidgets/lib/jquery/", package = "DT")
  )
  page <- htmltools::attachDependencies(
    page,
    htmltools::htmlDependency("jquery", "1.12.4", jquery_dir, script = "jquery.min.js"),
    append = TRUE
  )
  htmltools::htmlDependencies(page) <- rev(htmltools::htmlDependencies(page))
  page
}
