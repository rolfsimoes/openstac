% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{validate_functions}
\alias{validate_functions}
\alias{check_limit}
\alias{check_bbox}
\alias{check_page}
\alias{check_intersects}
\alias{check_collections}
\title{Validate HTTP input parameters for STAC and OGC API requests.}
\usage{
check_limit(limit, min, max)

check_bbox(bbox)

check_page(page)

check_intersects(intersects)

check_collections(collections)
}
\arguments{
\item{limit}{The limit parameter to be checked.}

\item{min}{The minimum allowed value for the limit parameter.}

\item{max}{The maximum allowed value for the limit parameter.}

\item{bbox}{The bbox parameter to be checked.}

\item{page}{The page parameter to be checked.}

\item{intersects}{The intersects parameter to be checked.}

\item{collections}{The collections parameter to be checked.}
}
\description{
This set of functions provides validation for various HTTP input
parameters commonly used in STAC and OGC API requests. If the
validation fails, these functions raises an API error with the
appropriate status code and error message. If the function
\code{api_error_handler} is set to handle erros in the API, it will
use these parameters to produce the error back to the user.
}
\details{
\itemize{

\item \code{check_limit}: Checks if the limit parameter is within the
specified range. If the limit is not an integer or is outside the
specified range, it raises an API error with the appropriate status
code.

\item \code{check_bbox}: Checks if the bbox parameter contains numeric
coordinates and has exactly four numbers. If the bbox parameter
is not valid, it raises an error with the appropriate status code.

\item \code{check_page}: Checks if the page parameter is a positive integer.
If the page parameter is not a positive integer, it raises an error
with the appropriate status code.

\item \code{check_intersects}: Checks if the intersects parameter is a
valid geometry. If the intersects parameter is not a valid GeoJSON
geometry, it raises an error with the appropriate status code.

\item \code{check_collections}: Checks if at least one collection is
provided. If no collections are provided, it raises an error with
the appropriate status code.

}
}
\seealso{
For more details on error handler: \code{\link[=api_error_handler]{api_error_handler()}}
}
