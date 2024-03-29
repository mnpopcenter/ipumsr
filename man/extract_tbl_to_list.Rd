% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{extract_tbl_to_list}
\alias{extract_tbl_to_list}
\title{Convert a tibble of extract definitions to a list}
\usage{
extract_tbl_to_list(extract_tbl, validate = TRUE)
}
\arguments{
\item{extract_tbl}{A \code{\link[tibble]{tbl_df}} (or \code{data.frame})
where each row contains the definition of one extract.}

\item{validate}{Logical (\code{TRUE} or \code{FALSE}) value indicating
whether to check that each row of \code{extract_tbl} contains a valid and
complete extract definition. Defaults to \code{TRUE}}
}
\value{
A list of length equal to the number of rows of \code{extract_tbl}.
}
\description{
Convert a \code{\link[tibble]{tbl_df}} (or \code{data.frame}) of extract
definitions, such as that returned by
\code{\link{get_recent_extracts_info_tbl}}, to a list of \code{ipums_extract}
objects. For an overview of ipumsr API functionality, see
\code{vignette("ipums-api", package = "ipumsr")}.
}
\examples{
\dontrun{
# Get tibble of recent extracts
tbl_of_last_10_extracts <- get_recent_extracts_info_tbl("usa")

# Filter down to extracts with "income" in the description
description_mentions_income <- grepl(
  "[Ii]ncome",
  tbl_of_last_10_extracts$description
)
income_extracts <- tbl_of_last_10_extracts[description_mentions_income, ]

# Convert tibble of extracts to list of extracts
income_extracts <- extract_tbl_to_list(income_extracts)

# Now it's easier to operate on those elements as extract objects:
revised_income_extract <- revise_extract_micro(
  income_extracts[[1]],
  samples_to_add = "us2018a"
)

submitted_revised_income_extract <- submit_extract(revised_income_extract)
}

}
\seealso{
Other ipums_api: 
\code{\link{define_extract_from_json}()},
\code{\link{define_extract_micro}()},
\code{\link{download_extract}()},
\code{\link{extract_list_to_tbl}()},
\code{\link{get_extract_info}()},
\code{\link{get_last_extract_info}()},
\code{\link{get_recent_extracts_info}},
\code{\link{ipums_data_collections}()},
\code{\link{is_extract_ready}()},
\code{\link{revise_extract_micro}()},
\code{\link{save_extract_as_json}()},
\code{\link{submit_extract}()},
\code{\link{wait_for_extract}()}
}
\concept{ipums_api}
