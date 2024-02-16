#' Deparsing Functions
#'
#' This set of functions provides functionality for converting R data
#' types back to HTTP input parameter strings.
#'
#' \itemize{
#'
#' \item `deparse_array`: Converts a vector to an array-like string.
#'
#' \item `deparse_datetime`: Converts a datetime object to an STAC
#'   interval-like string.
#'
#' \item `deparse_geojson`: Converts an R object to a GeoJSON string.
#'
#' }
#'
#' @param x The data to be converted.
#'
#' @param ... Additional arguments to be passed to `jsonlite::toJSON`.
#'
#' @seealso [jsonlite::toJSON()] For more details on what can be passed
#'   to `...` parameter.
#'
#' @name deparsing_functions
NULL
#' @rdname deparsing_functions
#' @export
deparse_array <- function(x) {
  if (is.null(x)) return(NULL)
  paste0(x, collapse = ",")
}
#' @rdname deparsing_functions
#' @export
deparse_datetime <- function(x) {
  if (is.null(x$start) && is.null(x$end) && is.null(x$exact))
    return(NULL)
  if (is.null(x$start) && is.null(x$end))
    return(as.character(x$exact))
  if (is.null(x$start)) x$start <- ".."
  if (is.null(x$end)) x$end <- ".."
  paste0(x$start, "/", x$end)
}
#' @rdname deparsing_functions
#' @export
deparse_geojson <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}
