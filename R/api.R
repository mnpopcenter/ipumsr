
# Exported functions ------------------------------------------------------


# > Define extract ----

#' Define an extract request object
#'
#' Define an extract request object to be submitted via the IPUMS API. For an
#' overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param collection The IPUMS data collection for the extract.
#' @param description Description of the extract.
#' @param samples Character vector of samples to include in the extract. Samples
#'   should be specified using the
#'   \href{https://usa.ipums.org/usa-action/samples/sample_ids}{Sample ID values}.
#' @param variables Character vector of variables to include in the extract.
#' @param data_format The desired format of the extract data file (one of
#'   "fixed_width", "csv", "stata", "spss", or "sas9").
#' @param data_structure One of "rectangular" or "hierarchical" (defaults to
#'   "rectangular")
#' @param rectangular_on For rectangular \code{data_structure}, should the data
#'   be rectangular on persons ("P") or households ("H")? Defaults to "P". If
#'   \code{data_structure} is hierarchical, this argument is ignored and set to
#'   missing automatically.
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' my_hierarchical_extract <- define_extract(
#'   collection = "usa",
#'   description = "A hierarchical example",
#'   samples = c("us2013a", "us2014a"),
#'   variables = c("YEAR", "AGE", "SEX", "RACE"),
#'   data_format = "csv",
#'   data_structure = "hierarchical"
#' )
#'
#' @export
define_extract <- function(collection,
                           description,
                           samples,
                           variables,
                           data_format = c("fixed_width", "csv", "stata",
                                           "spss", "sas9"),
                           data_structure = c("rectangular", "hierarchical"),
                           rectangular_on = c("P", "H")) {

  data_format <- match.arg(data_format)
  data_structure <- match.arg(data_structure)
  rectangular_on <- if(data_structure == "rectangular") {
    match.arg(rectangular_on)
  } else NA_character_

  stopifnot(is.character(collection), length(collection) == 1)
  stopifnot(is.character(description), length(description) == 1)
  stopifnot(is.character(data_structure), length(data_structure) == 1)
  stopifnot(is.character(rectangular_on), length(rectangular_on) == 1)
  stopifnot(is.character(data_format), length(data_format) == 1)
  stopifnot(is.character(samples))
  stopifnot(is.character(variables))

  extract <- new_ipums_extract(
    collection = collection,
    description = description,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format,
    samples = samples,
    variables = variables
  )

  extract <- validate_ipums_extract(extract)

  extract
}


# > Define extract from json ----

#' Create an extract object from a JSON-formatted definition
#'
#' Create an object of class "ipums_extract" based on an extract definition
#' formatted as JSON. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_json A JSON string, or the path to file containing JSON.
#' @inheritParams define_extract
#'
#' @family ipums_api
#' @return An object of class "ipums_extract".
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path, "usa")
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
define_extract_from_json <- function(extract_json, collection) {
  if (missing(collection)) {
    stop("`collection` is a required argument", call. = FALSE)
  }
  if (!collection %in% ipums_data_collections()$code_for_api) {
    stop(
      paste0(
        '"', collection, '"', " is not a valid code for an IPUMS data ",
        "collection. To see all valid collection codes, use ",
        "`ipums_data_collections()`"
      ),
      call. = FALSE
    )
  }
  list_of_extracts <- extract_list_from_json(
    extract_json,
    collection,
    validate = TRUE
  )
  if (length(list_of_extracts) != 1) {
    stop(
      paste0(
        "`extract_json` should contain the definition of one and only one ",
        "extract"
      ),
      call. = FALSE
    )
  }
  list_of_extracts[[1]]
}


# > Save extract as json ----

#' Save an extract definition to disk as JSON
#'
#' Save an extract definition to a JSON-formatted file. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams submit_extract
#' @param file File path at which to write the JSON-formatted extract
#'   definition.
#'
#' @details Note that this function only saves out the properties of an extract
#'   that are required to submit a new extract request, namely, the description,
#'   data structure, data format, samples, and variables.
#' @family ipums_api
#' @return The file path where the extract definition was written, invisibly.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' extract_json_path <- file.path(tempdir(), "usa_extract.json")
#' save_extract_as_json(my_extract, file = extract_json_path)
#'
#' copy_of_my_extract <- define_extract_from_json(extract_json_path, "usa")
#'
#' identical(my_extract, copy_of_my_extract)
#'
#' @export
save_extract_as_json <- function(extract, file) {
  extract_as_json <- extract_to_request_json(extract)
  writeLines(jsonlite::prettify(extract_as_json), con = file)
  invisible(file)
}


# > Submit extract ----

#' Submit an extract request via the IPUMS API
#'
#' Given an extract definition object, submit an extract request via the IPUMS
#' API, and return a modified copy of the extract object with the newly-assigned
#' extract number. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract An extract object created with \code{\link{define_extract}} or
#'   returned from another ipumsr API function.
#' @param api_key API key associated with your user account. Defaults to the
#'   value of environment variable "IPUMS_API_KEY".
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition and newly-assigned extract number of the submitted extract.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' # `submit_extract()` returns an ipums_extract object updated to include the
#' # extract number, so it is often useful to name the return object:
#' submitted_extract <- submit_extract(my_extract)
#'
#' # If you didn't capture the return object of submit_extract for your most
#' # recent extract, you can recover that information with:
#' submitted_extract <- get_last_extract_info("usa")
#'
#' # View the extract number
#' submitted_extract$number
#'
#' # Check if submitted extract is ready
#' is_extract_ready(submitted_extract) # returns TRUE or FALSE
#'
#' # Or have R check periodically until the extract is ready
#' downloadable_extract <- wait_for_extract(submitted_extract)
#' }
#'
#' @export
submit_extract <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- validate_ipums_extract(extract)

  response <- ipums_api_json_request(
    "POST",
    collection = extract$collection,
    path = NULL,
    body = extract_to_request_json(extract),
  )

  extract <- extract_list_from_json(
    response,
    collection = extract$collection
  )

  # extract_list_from_json() always returns a list of extracts, but in
  # this case there is always only one, so pluck it out
  extract <- extract[[1]]

  message(
    sprintf(
      "Successfully submitted %s extract number %d",
      format_collection_for_printing(extract$collection),
      extract$number
    )
  )

  extract
}


# > Get info on extract ----

#' Get information about a submitted extract
#'
#' Get information about a submitted extract via the IPUMS API. For an overview
#' of ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract One of:
#'   \itemize{
#'     \item{An object of class \code{ipums_extract}}
#'     \item{The data collection and extract number formatted as a single
#'           string of the form \code{"collection:number"}}
#'     \item{The data collection and extract number formatted as a vector of the
#'           form \code{c("collection", "number")}}
#'   }
#' The extract number does not need to be zero-padded (e.g., use \code{"usa:1"}
#' or \code{c("usa", "1")}, not \code{"usa:00001"} or \code{c("usa", "00001")}).
#' See Examples section below for examples of each form.
#' @inheritParams define_extract
#' @inheritParams download_extract
#'
#' @family ipums_api
#' @return An \code{ipums_extract} object.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Get info by supplying extract object:
#' get_extract_info(submitted_extract)
#'
#' # Get info by supplying the data collection and extract number, as a string:
#' get_extract_info("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # Get info by supplying the data collection and extract number, as a vector:
#' get_extract_info(c("usa", "1"))
#' }
#'
#' @export
get_extract_info <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {
  extract <- standardize_extract_identifier(extract)
  stopifnot(length(extract$collection) == 1)
  stopifnot(length(extract$number) == 1)
  if (is.na(extract$number)) {
    stop(
      "Extract number cannot be a missing value; please supply an ",
      "ipums_extract object returned by `submit_extract()`, or the data ",
      "collection and number of a submitted extract.",
      call. = FALSE
    )
  }
  collection <- tolower(extract$collection)
  response <- ipums_api_json_request(
    "GET",
    collection = collection,
    path = paste0("extracts/", extract$number),
    api_key = api_key
  )
  extract_list_from_json(response, collection)[[1]]
}


# > Wait for extract ----

#' Wait for extract to finish
#'
#' Wait for an extract to finish by periodically checking its status via the
#' IPUMS API and returning when the extract is ready to download. For an
#' overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams define_extract
#' @inheritParams download_extract
#' @inheritParams get_extract_info
#' @inheritParams submit_extract
#' @param initial_delay_seconds How many seconds to wait before first status
#'   check.
#' @param max_delay_seconds Maximum seconds to wait between status checks. The
#'   function doubles the wait time after each check, but will cap the wait
#'   wait time at this maximum value (300 seconds, or 5 minutes, by default).
#' @param timeout_seconds Maximum total number of seconds to continue waiting
#'   for the extract before throwing an error. Defaults to 10,800 seconds (three
#'   hours).
#' @param verbose If \code{TRUE}, the default, messages will be printed at the
#'   beginning of each wait interval with the current wait time, each time the
#'   status of the extract is checked, and when the extract is ready to
#'   download. Setting this argument to \code{FALSE} will silence these
#'   messages.
#'
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the extract
#'   definition and the URLs from which to download extract files.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Wait for extract by supplying extract object:
#' downloadable_extract <- wait_for_extract(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' downloadable_extract <- wait_for_extract("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' downloadable_extract <- wait_for_extract(c("usa", "1"))
#' }
#'
#' @export
wait_for_extract <- function(extract,
                             initial_delay_seconds = 0,
                             max_delay_seconds = 300,
                             timeout_seconds = 10800,
                             verbose = TRUE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {


  stopifnot(is.numeric(initial_delay_seconds))
  stopifnot(is.numeric(max_delay_seconds))
  stopifnot(is.null(timeout_seconds) || is.numeric(timeout_seconds))

  current_delay <- initial_delay_seconds
  is_timed_out <- FALSE
  is_finished <- FALSE
  is_error_state <- FALSE
  err_message <- "Unknown Error"

  wait_start <- Sys.time()

  while (!is_timed_out && !is_finished && !is_error_state) {
    if (current_delay > 0) {
      if (verbose) {
        message(paste("Waiting", current_delay, "seconds..."))
      }

      Sys.sleep(current_delay)
    }

    if (verbose) {
      message("Checking extract status...")
    }
    extract <- get_extract_info(extract, api_key)

    is_downloadable <- is_extract_ready(extract)

    is_failed <- !(is_downloadable ||
                     extract$status %in% c("queued", "started", "produced"))
    is_timed_out <- !is.null(timeout_seconds) &&
      as.numeric(Sys.time() - wait_start, units = "secs") > timeout_seconds


    if (is_downloadable) {
      if (verbose) {
        message("Extract ready to download")
      }
      is_finished <- TRUE
    } else if (is_failed) {
      err_message <- "Extract has finished, but is not in a downloadable state"
      is_error_state <- TRUE
    } else if (is_timed_out) {
      err_message <- "Max timeout elapsed"
    }

    if (current_delay == 0) {
      current_delay <- 10
    } else {
      current_delay <- min(c(current_delay * 2, max_delay_seconds))
    }
  }

  if (!is_finished) {
    stop(err_message)
  }

  extract
}


# > Check if downloadable ----

#' Is the extract ready to download?
#'
#' This function uses the IPUMS API to check whether the given extract is ready
#' to download, returning TRUE for extracts that are ready and FALSE for those
#' that are not. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @details
#' This function checks the "download_links" element of the supplied extract to
#' determine whether the extract files are available to download.
#' The "status" of a submitted extract is one of "queued", "started", "produced",
#' "canceled", "failed", or "completed". Only "completed" extracts can be ready
#' to download, but not all "completed" extracts are ready to download, because
#' extract files are subject to removal from the IPUMS servers 72 hours after
#' they first become available. Completed extracts older than 72 hours will
#' still have a "completed" status, but will return \code{FALSE} from
#' \code{is_extract_ready()}, because the extract files are no longer available.
#'
#' @inheritParams get_extract_info
#'
#' @family ipums_api
#' @return A logical vector of length one.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Check if extract is ready by supplying extract object:
#' is_extract_ready(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' is_extract_ready("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' is_extract_ready(c("usa", "1"))
#' }
#'
#' @export
is_extract_ready <- function(extract, api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract <- standardize_extract_identifier(extract)

  if (is.na(extract$number)) {
    stop(
      "ipums_extract object has a missing value in the 'number' field. If ",
      "an extract object is supplied, it must be a submitted extract with a ",
      "non-missing extract number.", call. = FALSE
    )
  }

  # First check if extract object already contains download info...
  if (inherits(extract, "ipums_extract") &&
      extract_is_completed_and_has_links(extract)) {
    return(TRUE)
  }

  # ... if it doesn't contain download info, make sure we have the latest
  # status by fetching it via the API and checking again
  extract <- get_extract_info(extract, api_key)

  extract_is_completed_and_has_links(extract)
}


# > Download extract ----

#' Download an IPUMS data extract
#'
#' Download an IPUMS data extract via the IPUMS API. For an overview of ipumsr
#' API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams get_extract_info
#' @inheritParams define_extract
#' @inheritParams submit_extract
#' @param download_dir In what folder should the downloaded files be saved?
#'   Defaults to current working directory.
#' @param overwrite Logical indicating whether to overwrite files that already
#'   exist. Defaults to \code{FALSE}.
#'
#' @family ipums_api
#' @return Invisibly, the path to the downloaded .xml DDI file.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submitted_extract <- submit_extract(my_extract)
#'
#' # Download extract by supplying extract object:
#' path_to_ddi_file <- download_extract(submitted_extract)
#'
#' # By supplying the data collection and extract number, as a string:
#' path_to_ddi_file <- download_extract("usa:1")
#' # Note that there is no space before or after the colon, and no zero-padding
#' # of the extract number.
#'
#' # By supplying the data collection and extract number, as a vector:
#' path_to_ddi_file <- download_extract(c("usa", "1"))
#' }
#'
#' @export
download_extract <- function(extract,
                             download_dir = getwd(),
                             overwrite = FALSE,
                             api_key = Sys.getenv("IPUMS_API_KEY")) {

  # Make sure we get latest extract status, but also make sure we don't check
  # the status twice
  is_ipums_extract_object <- inherits(extract, "ipums_extract")
  if (is_ipums_extract_object && extract_is_completed_and_has_links(extract)) {
    is_downloadable <- TRUE
  } else {
    extract <- get_extract_info(extract, api_key)
    is_downloadable <- extract_is_completed_and_has_links(extract)
  }

  if (!is_downloadable) {
    stop(
      paste0(
        format_collection_for_printing(extract$collection), " extract number ",
        extract$number, " is not ready to download"
      ),
      call. = FALSE
    )
  }

  download_dir <- normalizePath(download_dir, mustWork = FALSE)
  download_dir_doesnt_exist <- !dir.exists(download_dir)

  if (download_dir_doesnt_exist) {
    stop("The directory ", download_dir, " does not exist.", call. = FALSE)
  }

  ddi_url <- extract$download_links$ddi_codebook$url
  data_url <- extract$download_links$data$url
  ddi_file_path <- normalizePath(
    file.path(download_dir, basename(ddi_url)),
    mustWork = FALSE
  )
  data_file_path <- normalizePath(
    file.path(download_dir, basename(data_url)),
    mustWork = FALSE
  )

  ipums_api_download_request(ddi_url, ddi_file_path, overwrite, api_key)
  ipums_api_download_request(data_url, data_file_path, overwrite, api_key)

  message(
    paste0("DDI codebook file saved to ", ddi_file_path, "\nData file saved ",
           "to ", data_file_path)
  )

  invisible(ddi_file_path)
}


# > Revise extract definition ----

#' Revise an extract definition
#'
#' Revise an extract definition. If the supplied extract definition comes from
#' a previously submitted extract, this function will reset the definition to an
#' unsubmitted state. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract An object of class \code{ipums_extract}.
#' @param description The modified extract description. If NULL (the default),
#'   leave the description unchanged.
#' @param samples_to_add Samples to add to the extract definition, as a
#'   character vector. If NULL (the default), no samples will be added.
#' @param samples_to_remove Samples to remove from the extract definition, as a
#'   character vector. If NULL (the default), no samples will be removed.
#' @param vars_to_add Names of variables to add to the extract definition, as
#'   a character vector. If NULL (the default), no variables will be added.
#' @param vars_to_remove Names of variables to remove from the extract
#'   definition, as a character vector. If NULL (the default), no variables will
#'   be removed.
#' @param data_format The desired data file format for the modified extract
#'   definition. If NULL (the default), leave the data format unchanged.
#' @param data_structure The desired data structure ("rectangular" or
#'   "hierarchical") for the modified extract definition. If NULL (the default),
#'   leave the data structure unchanged.
#' @param rectangular_on If the modified extract will have a rectangular
#'   data structure, on what record type should it be rectangularized? If NULL,
#'   (the default), leave the \code{rectangular_on} field unchanged.
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing the modified
#'   extract definition.
#'
#' @examples
#' \dontrun{
#' old_extract <- get_extract_info("usa:33")
#'
#' revised_extract <- revise_extract(
#'   old_extract,
#'   samples_to_add = "us2018a",
#'   vars_to_add = "INCTOT"
#' )
#'
#' submitted_revised_extract <- submit_extract(revised_extract)
#' }
#'
#' @export
revise_extract <- function(extract,
                           description = NULL,
                           samples_to_add = NULL,
                           samples_to_remove = NULL,
                           vars_to_add = NULL,
                           vars_to_remove = NULL,
                           data_format = NULL,
                           data_structure = NULL,
                           rectangular_on = NULL) {

  extract <- copy_ipums_extract(extract)
  extract$description <- paste0("Revision of (", extract$description, ")")

  extract <- add_to_extract(extract, "samples", samples_to_add)
  extract <- remove_from_extract(extract, "samples", samples_to_remove)

  extract <- add_to_extract(extract, "variables", vars_to_add)
  extract <- remove_from_extract(extract, "variables", vars_to_remove)

  if (!is.null(description)) extract$description <- description
  if (!is.null(data_format)) extract$data_format <- data_format
  if (!is.null(data_structure)) extract$data_structure <- data_structure
  if (!is.null(rectangular_on)) extract$rectangular_on <- rectangular_on

  extract <- validate_ipums_extract(extract)

  extract
}

# > Get info on recent extracts ----

#' Get information on recent extracts
#'
#' Get information on up to ten recent extracts for a given IPUMS collection
#' via the IPUMS API, returned either as a list or tibble. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams define_extract
#' @param how_many Number of recent extracts for which you'd like information.
#' @inheritParams submit_extract
#'
#' @family ipums_api
#' @return For \code{get_recent_extracts_info_list()}, a list of
#'   \code{ipums_extract} objects. For \code{get_recent_extracts_info_tbl()},
#'   a \code{\link[tibble]{tbl_df}} with information on one extract in each row.
#'
#' @examples
#' \dontrun{
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_recent_extracts_info_list("usa")
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) print(extract$number)
#' }
#'
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
#' income_extracts <- tbl_of_last_10_extracts[description_mentions_income, ]
#'
#' # Convert tibble of extracts to list of extracts
#' income_extracts <- extract_tbl_to_list(income_extracts)
#'
#' # Now it's easier to operate on those elements as extract objects:
#' revised_income_extract <- revise_extract(
#'   income_extracts[[1]],
#'   samples_to_add = "us2018a"
#' )
#'
#' submitted_revised_income_extract <- submit_extract(revised_income_extract)
#' }
#'
#' @name get_recent_extracts_info
NULL

#' @rdname get_recent_extracts_info
#'
#' @export
get_recent_extracts_info_list <- function(collection,
                                          how_many = 10,
                                          api_key = Sys.getenv("IPUMS_API_KEY")) {

  response <- ipums_api_json_request(
    "GET",
    collection = collection,
    path = NULL,
    queries = list(limit = how_many),
    api_key = api_key
  )
  extract_list_from_json(response, collection)
}


#' @rdname get_recent_extracts_info
#' @export
get_recent_extracts_info_tbl <- function(collection,
                                         how_many = 10,
                                         api_key = Sys.getenv("IPUMS_API_KEY")) {

  extract_list <- get_recent_extracts_info_list(
    collection,
    how_many,
    api_key
  )

  extract_list_to_tbl(extract_list)
}


# > Get info on most recent extract ----

#' Get information on last extract
#'
#' Get information on your most recent extract for a given IPUMS data
#' collection, returned as an \code{ipums_extract} object. For an overview of
#' ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @inheritParams get_recent_extracts_info_list
#'
#' @family ipums_api
#' @return An object of class \code{ipums_extract} containing information on
#'   your most recent extract.
#'
#' @examples
#' my_extract <- define_extract("usa", "Example", "us2013a", "YEAR")
#'
#' \dontrun{
#' submit_extract(my_extract)
#'
#' # Oops, forgot to capture the return object from submit_extract. Grab it with:
#' submitted_extract <- get_last_extract_info("usa")
#'
#' # View the extract number
#' submitted_extract$number
#'
#' # Check if submitted extract is ready
#' is_extract_ready(submitted_extract) # returns TRUE or FALSE
#'
#' # Or have R check periodically until the extract is ready
#' downloadable_extract <- wait_for_extract(submitted_extract)
#' }
#'
#' @export
get_last_extract_info <- function(collection,
                                  api_key = Sys.getenv("IPUMS_API_KEY")) {
  get_recent_extracts_info_list(collection, 1, api_key)[[1]]
}


# > Convert extract tbl to list ----

#' Convert a tibble of extract definitions to a list
#'
#' Convert a \code{\link[tibble]{tbl_df}} (or \code{data.frame}) of extract
#' definitions, such as that returned by
#' \code{\link{get_recent_extracts_info_tbl}}, to a list of \code{ipums_extract}
#' objects. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_tbl A \code{\link[tibble]{tbl_df}} (or \code{data.frame})
#'   where each row contains the definition of one extract.
#' @param validate Logical (\code{TRUE} or \code{FALSE}) value indicating
#'   whether to check that each row of \code{extract_tbl} contains a valid and
#'   complete extract definition. Defaults to \code{TRUE}
#'
#' @family ipums_api
#' @return A list of length equal to the number of rows of \code{extract_tbl}.
#'
#' @examples
#' \dontrun{
#' # Get tibble of recent extracts
#' tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")
#'
#' # Filter down to extracts with "income" in the description
#' description_mentions_income <- grepl(
#'   "[Ii]ncome",
#'   tbl_of_last_10_extracts$description
#' )
#' income_extracts <- tbl_of_last_10_extracts[description_mentions_income, ]
#'
#' # Convert tibble of extracts to list of extracts
#' income_extracts <- extract_tbl_to_list(income_extracts)
#'
#' # Now it's easier to operate on those elements as extract objects:
#' revised_income_extract <- revise_extract(
#'   income_extracts[[1]],
#'   samples_to_add = "us2018a"
#' )
#'
#' submitted_revised_income_extract <- submit_extract(revised_income_extract)
#' }
#'
#' @export
extract_tbl_to_list <- function(extract_tbl, validate = TRUE) {
  expected_names <- names(new_ipums_extract())
  unexpected_names <- setdiff(names(extract_tbl), expected_names)
  if (length(unexpected_names) > 0) {
    stop(
      "Unexpected names in `extract_tbl`: ",
      paste0('"', unexpected_names, '"', collapse = ", "),
      call. = FALSE
    )
  }
  extract_list <- purrr::pmap(extract_tbl, new_ipums_extract)
  if (validate) {
    extract_list <- purrr::walk(extract_list, validate_ipums_extract)
  }
  extract_list
}


# > Convert extract list to tbl ----

#' Convert a list of extract definitions to a tibble
#'
#' Convert a list of \code{ipums_extract} objects to a
#' \code{\link[tibble]{tbl_df}} in which each row contains the definition of one
#' extract. For an overview of ipumsr API functionality, see
#' \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @param extract_list A list of \code{ipums_extract} objects.
#'
#' @family ipums_api
#' @return A \code{\link[tibble]{tbl_df}} with number of rows equal to the
#'   length of \code{extract_list}, in which each rows contains the definition
#'   of one extract.
#'
#' @examples
#' \dontrun{
#' # Get list of recent extracts
#' list_of_last_10_extracts <- get_recent_extracts_info_list("usa")
#'
#' # Print the extract number for extracts that are downloadable:
#' for (extract in list_of_last_10_extracts) {
#'   if (is_extract_ready(extract)) print(extract$number)
#' }
#'
#' # Convert list of extracts to tibble of extracts to view in a tabular format
#' extract_list_to_tbl(list_of_last_10_extracts)
#' }
#'
#' @export
extract_list_to_tbl <- function(extract_list) {
  unclassed_extract_list <- purrr::map(
    extract_list,
    function(x) {
      if (is.character(x$samples)) x$samples <- list(x$samples)
      if (is.character(x$variables)) x$variables <- list(x$variables)
      x$download_links <- list(x$download_links)
      unclass(x)
    }
  )
  if (length(unclassed_extract_list) == 1) {
    return(do.call(tibble::tibble, unclassed_extract_list[[1]]))
  }
  do.call(dplyr::bind_rows, unclassed_extract_list)
}


# > List data collections ----

#' List IPUMS data collections
#'
#' List IPUMS data collections with corresponding codes used by the IPUMS API.
#' Note that some data collections do not yet have API support. For an overview
#' of ipumsr API functionality, see \code{vignette("ipums-api", package = "ipumsr")}.
#'
#' @family ipums_api
#' @return A \code{\link[tibble]{tbl_df}} with two columns containing the
#'   full collection name and the corresponding code used by the IPUMS API.
#'
#' @examples
#' # Print a tibble of all IPUMS data collections:
#' ipums_data_collections()
#'
#' @export
ipums_data_collections <- function() {
  tibble::tribble(
    ~collection_name, ~code_for_api,
    "IPUMS USA", "usa",
    "IPUMS CPS", "cps",
    "IPUMS International", "ipumsi",
    "IPUMS NHGIS", "nhgis",
    "IPUMS AHTUS", "ahtus",
    "IPUMS MTUS", "mtus",
    "IPUMS ATUS", "atus",
    "IPUMS DHS", "dhs",
    "IPUMS Higher Ed", "highered",
    "IPUMS MEPS", "meps",
    "IPUMS NHIS", "nhis",
    "IPUMS PMA", "pma"
  )
}


# Non-exported functions --------------------------------------------------
new_ipums_extract <- function(collection = NA_character_,
                              description = NA_character_,
                              data_structure = NA_character_,
                              rectangular_on = NA_character_,
                              data_format = NA_character_,
                              samples = NA_character_,
                              variables = NA_character_,
                              submitted = FALSE,
                              download_links = EMPTY_NAMED_LIST,
                              number = NA_integer_,
                              status = "unsubmitted") {

  out <- list(
    collection = collection,
    description = description,
    data_structure = data_structure,
    rectangular_on = rectangular_on,
    data_format = data_format,
    samples = samples,
    variables = variables,
    submitted = submitted,
    download_links = download_links,
    number = number,
    status = status
  )

  structure(
    out,
    class = c("ipums_extract", class(out))
  )
}


standardize_extract_identifier <- function(extract) {
  if (inherits(extract, "ipums_extract")) return(extract)
  if (length(extract) == 1) {
    extract <- fostr_split(extract, ":")[[1]]
  }

  if (length(extract) != 2) {
    stop(
      paste0(
        "Expected extract to be either an `ipums_extract` object, a ",
        "length-two vector where the first element is an IPUMS collection ",
        "and the second is an extract number, or a single string with a ':' ",
        "separating the collection from the extract number."
      ),
      call. = FALSE
    )
  }
  collection <- extract[[1]]
  number <- suppressWarnings(as.numeric(extract[[2]]))
  if (is.na(number)) {
    stop("Expected extract number to be a number", call. = FALSE)
  }

  list(collection = collection, number = number)
}


validate_ipums_extract <- function(x) {

  must_be_non_missing <- c("collection", "description", "data_structure",
                           "data_format", "samples", "variables")

  is_missing <- purrr::map_lgl(must_be_non_missing, ~any(is.na(x[[.]])))

  if (any(is_missing)) {
    stop(
      "The following elements of an ipums_extract must not contain missing ",
      "values: ",
      paste0(must_be_non_missing[is_missing], collapse = ", "),
      call. = FALSE
    )
  }

  stopifnot(x$data_format %in% c("fixed_width", "csv","stata", "spss", "sas9"))
  stopifnot(x$data_structure %in% c("rectangular", "hierarchical"))

  if (x$data_structure == "rectangular" & !x$rectangular_on %in% c("H", "P")) {
    stop("If `data_structure` is 'rectangular', `rectangular_on` must be one ",
         "of 'H' or 'P'")
  }

  if (x$data_structure == "hierarchical" & !is.na(x$rectangular_on)) {
    stop("If `data_structure` is 'hierarchical', `rectangular_on` must be ",
         "missing")
  }

  x
}


#' @export
print.ipums_extract <- function(extract) {
  to_cat <- paste0(
    ifelse(extract$submitted, "Submitted ", "Unsubmitted "),
    format_collection_for_printing(extract$collection),
    " extract ", ifelse(extract$submitted, paste0("number ", extract$number), ""),
    "\n", print_truncated_vector(extract$description, "Description: ", FALSE),
    "\n", print_truncated_vector(extract$samples, "Samples: "),
    "\n", print_truncated_vector(extract$variables, "Variables: ")
  )

  cat(to_cat)

  invisible(extract)
}


format_collection_for_printing <- function(collection) {
  collection_info <- dplyr::filter(
    ipums_data_collections(),
    code_for_api == collection
  )

  if (nrow(collection_info) == 0) {
    return(UNKNOWN_DATA_COLLECTION_LABEL)
  }

  collection_info$collection_name
}


UNKNOWN_DATA_COLLECTION_LABEL <- "Unknown data collection"


print_truncated_vector <- function(x, label = NULL, include_length = TRUE) {
  max_width <- min(getOption("width"), 80)
  max_width <- max(max_width, 20) # don't allow width less than 20
  full_list <- paste0(x, collapse = ", ")
  untruncated <- ifelse(
    include_length,
    paste0(label, "(", length(x), " total) ", full_list),
    paste0(label, full_list)
  )
  if (nchar(untruncated) > max_width) {
    return(paste0(substr(untruncated, 1, max_width - 3), "..."))
  }
  untruncated
  # will_be_truncated <- length(x) > truncate_at
  # x <- head(x, truncate_at)
  # out <- paste0(x, collapse = ", ")
  # if (will_be_truncated) {
  #   return(paste0(out, "..."))
  # }
  # out
}


extract_to_request_json <- function(extract) {
  if (is.na(extract$description)) {
    extract$description <- ""
  }
  if (is.na(extract$data_format)) {
    extract$data_format <- ""
  }
  request_list <- list(
    description = extract$description,
    data_structure = format_data_structure_for_json(
      extract$data_structure,
      extract$rectangular_on
    ),
    data_format = extract$data_format,
    samples = format_samples_for_json(extract$samples),
    variables = format_variables_for_json(extract$variables)
  )
  jsonlite::toJSON(request_list, auto_unbox = TRUE)
}


format_samples_for_json <- function(samples) {
  if (length(samples) == 1 && is.na(samples)) {
    return(EMPTY_NAMED_LIST)
  }
  sample_spec <- purrr::map(seq_along(samples), ~ EMPTY_NAMED_LIST)
  setNames(sample_spec, samples)
}


format_variables_for_json <- function(variables) {
  if (length(variables) == 1 && is.na(variables)) {
    return(EMPTY_NAMED_LIST)
  }
  var_spec <- purrr::map(seq_along(variables), ~ EMPTY_NAMED_LIST)
  var_spec <- setNames(var_spec, variables)
}


format_data_structure_for_json <- function(data_structure, rectangular_on) {
  if (is.na(data_structure)) {
    return(EMPTY_NAMED_LIST)
  } else if (data_structure == "rectangular") {
    return(list(rectangular = list(on = rectangular_on)))
  } else if (data_structure == "hierarchical") {
    return(list(hierarchical = EMPTY_NAMED_LIST))
  } else {
    return(EMPTY_NAMED_LIST)
  }
}


#' Writes the given url to file_path. Returns the file path of the
#' downloaded data. Raises an error if the request is not successful.
#' @noRd
ipums_api_download_request <- function(url,
                                       file_path,
                                       overwrite,
                                       api_key = Sys.getenv("IPUMS_API_KEY")) {


  file_already_exists <- file.exists(file_path)

  if (file.exists(file_path) && !overwrite) {
    stop("File", file_path, " already exists. If you want to overwrite, set ",
         "`overwrite` to TRUE.", call. = FALSE)
  }

  response <- httr::GET(
    url,
    httr::user_agent(
      paste0(
        "https://github.com/mnpopcenter/ipumsr ",
        as.character(packageVersion("ipumsr"))
      )
    ),
    add_user_auth_header(api_key),
    httr::write_disk(file_path, overwrite = TRUE)
  )

  if (httr::http_status(response)$category != "Success") {
    stop(paste("Could not save", url, "to", file_path))
  }

  return(file_path)
}


#' Helper function to form, submit, and receive responses of requests expecting
#'   a JSON response.
#'
#' @param verb "GET" or "POST"
#' @param collection The IPUMS data collection for the extract.
#' @param path Extensions to add to the base url.
#' @param body The body of the request (e.g. the extract definition), if
#'   relevant. Defaults to FALSE, which creates a body-less request.
#' @param queries A named list of key value pairs to be added to the standard
#'   query in the call to httr::modify_url.
#'
#' @return If the request returns a JSON response, this function returns a
#'   length-one character vector containing the response from the API
#'   formatted as JSON. Otherwise, the function throws an error.
#'
#' @noRd
ipums_api_json_request <- function(verb,
                                   collection,
                                   path,
                                   body = FALSE,
                                   queries = NULL,
                                   api_key = Sys.getenv("IPUMS_API_KEY")) {

  queries_is_null_or_named_list <- is.null(queries) ||
    is.list(queries) && !is.null(names(queries)) && !any(names(queries) == "")

  if (!queries_is_null_or_named_list) {
    stop("`queries` argument must be NULL or a named list")
  }

  api_url <- httr::modify_url(
    microdata_api_base_url(),
    path = path,
    query = c(
      list(collection = collection, version = microdata_api_version()),
      queries
    )
  )

  res <- httr::VERB(
    verb = verb,
    url = api_url,
    body = body,
    httr::user_agent(
      paste0(
        "https://github.com/mnpopcenter/ipumsr ",
        as.character(packageVersion("ipumsr"))
      )
    ),
    httr::content_type_json(),
    add_user_auth_header(api_key)
  )

  if (httr::http_status(res)$category != "Success") {
    if (httr::status_code(res) == 400) {
      tryCatch(
        error_details <- parse_400_error(res),
        error = function(cond) {
          stop(
            "Received error from server (status code 400), but could not ",
            "parse response for more details."
          )
        }
      )
      stop(error_details, call. = FALSE)
    } else if (httr::status_code(res) == 404) {
      if (fostr_detect(path, "^extracts/\\d+$")) {
        extract_number <- as.numeric(fostr_split(path, "/")[[1]][[2]])
        most_recent_extract <- get_recent_extracts_info_list(
          collection,
          how_many = 1
        )
        most_recent_extract_number <- most_recent_extract[[1]]$number
        if (extract_number > most_recent_extract_number) {
          coll <- format_collection_for_printing(collection)
          stop(coll, " extract number ", extract_number, " does not exist; ",
               "most recent extract number is ", most_recent_extract_number,
               call. = FALSE)
        }
      }
      stop("URL not found", call. = FALSE)
    } else if (httr::status_code(res) %in% 500:599) {
      stop(
        sprintf(
          "Extract API request failed: %s [%s]\n%s",
          api_url,
          httr::status_code(res),
          httr::content(res, "text")
        ),
        call. = FALSE
      )
    } else { # other non-success codes, e.g. 300s
      stop(
        sprintf(
          "Extract API request failed: %s [%s]\n%s",
          api_url,
          httr::status_code(res),
          httr::content(res, "text")
        ),
        call. = FALSE
      )
    }
  }

  if (httr::http_type(res) != "application/json") {
    stop("Extract API did not return json", call. = FALSE)
  }

  httr::content(res, "text")

}

extract_list_from_json <- function(extracts_as_json,
                                   collection,
                                   validate = FALSE) {
  list_of_extract_info <- jsonlite::fromJSON(
    extracts_as_json,
    simplifyVector = FALSE
  )

  # The response only has names when it contains info on a single extract. In
  #   that case, we want to make sure this function returns an unnamed list of
  #   length one, to ensure consistency in the structure of the return value.
  list_contains_info_on_single_extract <- !is.null(names(list_of_extract_info))

  if (list_contains_info_on_single_extract) {
    list_of_extract_info <- list(list_of_extract_info)
  }

  purrr::map(
    list_of_extract_info,
    function(x) {
      out <- new_ipums_extract(
        collection = collection,
        description = x$description,
        data_structure = names(x$data_structure),
        rectangular_on = ifelse(
          names(x$data_structure) == "rectangular",
          x$data_structure$rectangular$on,
          NA_character_
        ),
        data_format = x$data_format,
        samples = names(x$samples),
        variables = names(x$variables),
        submitted = ifelse("number" %in% names(x), TRUE, FALSE),
        download_links = if ("download_links" %in% names(x)) {
          x$download_links
        } else EMPTY_NAMED_LIST,
        number = ifelse("number" %in% names(x), x$number, NA_integer_),
        status = ifelse("status" %in% names(x), x$status, "unsubmitted")
      )
      if (validate) validate_ipums_extract(out)
      out
    }
  )
}


parse_400_error <- function(res) {
  response_content <- jsonlite::fromJSON(
    httr::content(res, "text"),
    simplifyVector = FALSE
  )
  response_detail <- response_content$detail
  response_detail <- unlist(response_detail)
  error_message <- paste0(
    "Received status code 400, with the following details:\n\n",
    paste0(response_detail, collapse = "\n\n")
  )
  return(error_message)
}

json_to_extract <- function(json, collection) {
  json_as_data_frame <- jsonlite::fromJSON(json)
}


extract_is_completed_and_has_links <- function(extract) {
  status <- extract$status
  download_links <- extract$download_links

  has_url <- function(links, name) {
    return (is.list(links[[name]]) && is.character(links[[name]][["url"]]))
  }

  status == "completed" && has_url(download_links, "ddi_codebook") &&
    has_url(download_links, "data")
}


add_user_auth_header <- function(api_key) {
  httr::add_headers("Authorization" = api_key)
}


copy_ipums_extract <- function(extract) {
  extract$submitted <- FALSE
  extract$download_links <- EMPTY_NAMED_LIST
  extract$number <- NA_integer_
  extract$status <- "unsubmitted"

  extract
}


add_to_extract <- function(extract, samples_or_variables, names_to_add) {
  if (is.null(names_to_add)) {
    return(extract)
  }
  if (any(names_to_add %in% extract[[samples_or_variables]])) {
    warning(
      "The following ", samples_or_variables, " are already included in the ",
      "supplied extract definition, and thus will not be added: ",
      paste0(
        intersect(names_to_add, extract[[samples_or_variables]]),
        collapse = ", "
      ),
      call. = FALSE
    )
    names_to_add <- setdiff(names_to_add, extract[[samples_or_variables]])
  }
  extract[[samples_or_variables]] <- c(
    extract[[samples_or_variables]],
    names_to_add
  )
  extract
}


remove_from_extract <- function(extract,
                                samples_or_variables,
                                names_to_remove) {
  if (is.null(names_to_remove)) {
    return(extract)
  }
  if (!all(names_to_remove %in% extract[[samples_or_variables]])) {
    warning(
      "The following ", samples_or_variables, " are not included in the ",
      "supplied extract definition, and thus will not be removed: ",
      paste0(
        setdiff(names_to_remove, extract[[samples_or_variables]]),
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  extract[[samples_or_variables]] <- setdiff(
    extract[[samples_or_variables]],
    names_to_remove
  )
  extract
}


microdata_api_base_url <- function() {
  api_url <- Sys.getenv("IPUMS_API_URL")
  if (api_url == "") return("https://api.ipums.org/extracts/")
  api_url
}


microdata_api_version <- function() {
  api_version <- Sys.getenv("IPUMS_API_VERSION")
  if (api_version == "") return("v1")
  api_version
}





EMPTY_NAMED_LIST <- setNames(list(), character(0))
