#' @keywords internal
deparse_array <- function(x) {
  if (is.null(x)) return(NULL)
  paste0(x, collapse = ",")
}
#' @keywords internal
deparse_datetime <- function(x) {
  if (is.null(x$start) && is.null(x$end) && is.null(x$exact))
    return(NULL)
  if (is.null(x$start) && is.null(x$end))
    return(as.character(x$exact))
  if (is.null(x$start)) x$start <- ".."
  if (is.null(x$end)) x$end <- ".."
  paste0(x$start, "/", x$end)
}

deparse_geojson <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}
