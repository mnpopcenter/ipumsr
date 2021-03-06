% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/micro_read_yield.r
\name{read_ipums_micro_yield}
\alias{read_ipums_micro_yield}
\alias{read_ipums_micro_list_yield}
\alias{IpumsLongYield}
\alias{IpumsListYield}
\title{Read data from an IPUMS extract (in yields)}
\usage{
read_ipums_micro_yield(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)

read_ipums_micro_list_yield(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)
}
\arguments{
\item{ddi}{Either a filepath to a DDI xml file downloaded from
the website, or a \code{ipums_ddi} object parsed by \code{\link{read_ipums_ddi}}}

\item{vars}{Names of variables to load. Accepts a character vector of names, or
\code{\link{dplyr_select_style}} conventions. For hierarchical data, the
rectype id variable will be added even if it is not specified.}

\item{data_file}{Specify a directory to look for the data file.
If left empty, it will look in the same directory as the DDI file.}

\item{verbose}{Logical, indicating whether to print progress information
to console.}

\item{var_attrs}{Variable attributes to add from the DDI, defaults to
adding all (val_labels, var_label and var_desc). See
\code{\link{set_ipums_var_attributes}} for more details.}

\item{lower_vars}{Only if reading a DDI from a file, a logical indicating
whether to convert variable names to lowercase (default is FALSE, in line
with IPUMS conventions). Note that this argument will be ignored if
argument \code{ddi} is an \code{ipums_ddi} object rather than a file path.
See \code{\link{read_ipums_ddi}} for converting variable names to lowercase
when reading in the DDI.}
}
\value{
A HipYield R6 object (See 'Details' for more information)
}
\description{
Reads a dataset downloaded from the IPUMS extract system, but does
so by returning an object that can read a group of lines at a time.
This is a more flexible way to read data in chunks than
the functions like \code{\link{read_ipums_micro_chunked}}, allowing
you to do things like reading parts of multiple files at the same time
and resetting from the beginning more easily than with the chunked
functions. \strong{Note that while other \code{read_ipums_micro*} functions
can read from .csv(.gz) or .dat(.gz) files, these functions can only read
from .dat(.gz) files.}
}
\details{
These functions return an IpumsYield R6 object which have the following
methods:
\itemize{
\item \code{yield(n = 10000)} A function to read the next 'yield' from the data,
  returns a `tbl_df` (or list of `tbl_df` for `hipread_list_yield()`)
  with up to n rows (it will return NULL if no rows are left, or all
  available ones if less than n are available).
\item \code{reset()} A function to reset the data so that the next yield will
  read data from the start.
\item \code{is_done()} A function that returns whether the file has been completely
  read yet or not.
\item \code{cur_pos} A property that contains the next row number that will be
   read (1-indexed).
}
}
\examples{
# An example using "long" data
long_yield <- read_ipums_micro_yield(ipums_example("cps_00006.xml"))
# Get first 10 rows
long_yield$yield(10)
# Get 20 more rows now
long_yield$yield(20)
# See what row we're on now
long_yield$cur_pos
# Reset to beginning
long_yield$reset()
# Read the whole thing in chunks and count Minnesotans
total_mn <- 0
while (!long_yield$is_done()) {
  cur_data <- long_yield$yield(1000)
  total_mn <- total_mn + sum(as_factor(cur_data$STATEFIP) == "Minnesota")
}
total_mn

# Can also read hierarchical data as list:
list_yield <- read_ipums_micro_list_yield(ipums_example("cps_00006.xml"))
list_yield$yield(10)

}
\seealso{
Other ipums_read: 
\code{\link{read_ipums_micro_chunked}()},
\code{\link{read_ipums_micro}()},
\code{\link{read_ipums_sf}()},
\code{\link{read_nhgis}()},
\code{\link{read_terra_area}()},
\code{\link{read_terra_micro}()},
\code{\link{read_terra_raster}()}
}
\concept{ipums_read}
\section{Super classes}{
\code{\link[hipread:HipYield]{hipread::HipYield}} -> \code{\link[hipread:HipLongYield]{hipread::HipLongYield}} -> \code{IpumsLongYield}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{IpumsLongYield$new()}}
\item \href{#method-yield}{\code{IpumsLongYield$yield()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="hipread" data-topic="HipYield" data-id="is_done">}\href{../../hipread/html/HipYield.html#method-is_done}{\code{hipread::HipYield$is_done()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hipread" data-topic="HipYield" data-id="reset">}\href{../../hipread/html/HipYield.html#method-reset}{\code{hipread::HipYield$reset()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IpumsLongYield$new(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-yield"></a>}}
\if{latex}{\out{\hypertarget{method-yield}{}}}
\subsection{Method \code{yield()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IpumsLongYield$yield(n = 10000)}\if{html}{\out{</div>}}
}

}
}
\section{Super classes}{
\code{\link[hipread:HipYield]{hipread::HipYield}} -> \code{\link[hipread:HipListYield]{hipread::HipListYield}} -> \code{IpumsListYield}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{IpumsListYield$new()}}
\item \href{#method-yield}{\code{IpumsListYield$yield()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="hipread" data-topic="HipYield" data-id="is_done">}\href{../../hipread/html/HipYield.html#method-is_done}{\code{hipread::HipYield$is_done()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="hipread" data-topic="HipYield" data-id="reset">}\href{../../hipread/html/HipYield.html#method-reset}{\code{hipread::HipYield$reset()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IpumsListYield$new(
  ddi,
  vars = NULL,
  data_file = NULL,
  verbose = TRUE,
  var_attrs = c("val_labels", "var_label", "var_desc"),
  lower_vars = FALSE
)}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-yield"></a>}}
\if{latex}{\out{\hypertarget{method-yield}{}}}
\subsection{Method \code{yield()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{IpumsListYield$yield(n = 10000)}\if{html}{\out{</div>}}
}

}
}
