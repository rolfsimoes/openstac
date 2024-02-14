#' Link helper functions
#'
#' These functions modify links in an document by adding new links or
#' updating existing ones.
#'
#' \itemize{
#'
#' \item `add_link`: This function adds a new link to the document by
#'   appending it to the existing links.
#'
#' \item `update_link`: This function updates an existing link in the
#'   document by replacing it with a new link. The existing link is
#'   identified by its relationship (rel). If multiple links with
#'   the same relationship exist, all of them are replaced.
#'
#' }
#'
#' @param doc The document to which the link will be added.
#'
#' @param rel The relationship of the link (e.g., "self", "child").
#'
#' @param href The URL of the linked resource.
#'
#' @param ... Additional attributes to include in the link.
#'
#' @return The updated document with the new link added.
#'
#' @name link_functions
NULL
#' @keywords internal
make_url <- function(host, ...) {
  dots <- c(...)
  segments <- unname(dots)
  params <- NULL
  if (!is.null(names(dots))) {
    segments <- unname(dots[names(dots) == ""])
    params <- dots[names(dots) != ""]
  }
  path <- paste0(segments, collapse = "/")
  href <- paste0(host, path)
  query <- paste(names(params), unname(params), sep = "=", collapse = "&")
  if (query != "") href <- paste0(href, "?", query)
  href
}
#' @keywords internal
new_link <- function(rel, href, ...) {
  dots <- list(...)
  not_null <- !vapply(dots, is.null, logical(1), USE.NAMES = FALSE)
  c(list(rel = rel, href = href), dots[not_null])
}
#' @rdname link_functions
#' @export
add_link <- function(doc, rel, href, ...) {
  doc$links <- c(doc$links, list(new_link(rel, href, ...)))
  doc
}
#' @rdname link_functions
#' @export
update_link <- function(doc, rel, href, ...) {
  select <- vapply(doc$links, \(x) !is.null(x$rel) && x$rel != rel)
  doc$links <- doc$links[select]
  doc$links <- c(doc$links, list(new_link(rel, href, ...)))
  doc
}
