#' @rdname doc_handling
#' @export
doc_landing_page.stac <- function(api, req) {
  doc <- NextMethod("doc_loading_page", api)
  doc <- c(list(
    stac_version = api$stac_version,
    type = "Catalog",
    id = api$id,
    conformsTo = api$conforms_to
  ), doc)
  doc <- links_landing_page(doc, api, req)
  doc
}
#' @rdname doc_handling
#' @export
doc_conformance.stac <- function(api, req) {
  doc <- NextMethod("doc_conformance", api)
  # A list of all conformance classes specified in a standard that the
  # server conforms to.
  conforms_to <- api$conforms_to
  if (is.null(api$conforms_to))
    conforms_to <- c(
      "https://api.stacspec.org/v1.0.0/core",
      "https://api.stacspec.org/v1.0.0/collections",
      "https://api.stacspec.org/v1.0.0/item-search",
      "https://api.stacspec.org/v1.0.0/ogcapi-features"
    )
  doc <- list(conformsTo = c(doc$conformsTo, conforms_to))
  doc
}
#' @rdname doc_handling
#' @export
doc_collections.stac <- function(api, req) {
  NextMethod("doc_collections", api)
}
#' @rdname doc_handling
#' @export
doc_collection.stac <- function(api, req, collection_id) {
  NextMethod("doc_collection", api)
}
#' @rdname doc_handling
#' @export
doc_items.stac <- function(api,
                           req,
                           collection_id,
                           limit,
                           bbox,
                           datetime,
                           page) {
  NextMethod("doc_items", api)
}
#' @rdname doc_handling
#' @export
doc_item.stac <- function(api, req, collection_id, item_id) {
  NextMethod("doc_item", api)
}
#' @rdname doc_handling
#' @export
doc_search.stac <- function(api,
                            req,
                            limit,
                            bbox,
                            datetime,
                            intersects,
                            ids,
                            collections,
                            page) {
  db <- get_db(api)
  check_collection_in_db(db, collections)
  doc <- db_search(
    db = db,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
  doc <- links_search(
    doc = doc,
    api = api,
    req = req,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    intersects = intersects,
    ids = ids,
    collections = collections,
    page = page
  )
  doc
}
#' @keywords internal
#' @export
links_landing_page.stac <- function(doc, api, req) {
  doc <- NextMethod("links_landing_page", api)
  host <- get_host(api, req)
  doc <- add_link(
    doc = doc,
    rel = "search",
    href = make_url(host, "/search"),
    type = "application/geo+json",
    title = "STAC search",
    method = "GET"
  )
  doc <- add_link(
    doc = doc,
    rel = "search",
    href = make_url(host, "/search"),
    type = "application/geo+json",
    title = "STAC search",
    method = "POST"
  )
  doc
}
#' @keywords internal
#' @export
links_collections.stac <- function(doc, api, req) {
  NextMethod("links_collections", api)
}
#' @keywords internal
#' @export
links_collection.stac <- function(doc, api, req) {
  NextMethod("links_collection", api)
}
#' @keywords internal
#' @export
links_items.stac <- function(doc,
                             api,
                             req,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page) {
  NextMethod("links_items", api)
}
#' @keywords internal
#' @export
links_item.stac <- function(doc, api, req) {
  NextMethod("links_item", api)
}
#' @keywords internal
#' @export
links_search.stac <- function(doc,
                              api,
                              req,
                              limit,
                              bbox,
                              datetime,
                              intersects,
                              ids,
                              collections,
                              page) {
  # update item links
  doc$features <- lapply(doc$features, \(item) {
    links_item(item, api, req)
  })
  # basic links
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  # add navigation links
  method <- get_method(req)
  if (method == "GET") {
    doc <- links_navigation(
      doc = doc,
      api = api,
      req = req,
      endpoint = "/search",
      limit = limit,
      page = page,
      bbox = deparse_array(bbox),
      datetime = deparse_datetime(datetime),
      ids = ids,
      collections = collections,
      type = "application/geo+json"
    )
  } else if (method == "POST") {
    doc <- links_navigagion_post(
      doc = doc,
      api = api,
      req = req,
      endpoint = "/search",
      limit = limit,
      page = page,
      type = "application/geo+json",
      merge = TRUE
    )
  }
  doc
}
