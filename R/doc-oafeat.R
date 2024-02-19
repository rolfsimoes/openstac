#' @rdname doc_handling
#' @export
doc_landing_page.oafeat <- function(api, req) {
  doc <- list(title = api$title, description = api$description)
  doc <- links_landing_page(doc, api, req)
  doc
}
#' @rdname doc_handling
#' @export
doc_conformance.oafeat <- function(api, req) {
  # A list of all conformance classes specified in a standard that the
  # server conforms to.
  conforms_to <- api$conforms_to
  if (is.null(api$conforms_to))
    conforms_to <- c(
      "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
      "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
      "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
    )
  doc <- list(conformsTo = conforms_to)
  doc
}
#' @rdname doc_handling
#' @export
doc_collections.oafeat <- function(api, req) {
  db <- get_db(api)
  doc <- list(collections = db_collections(db))
  doc <- links_collections(doc, api, req)
  doc
}
#' @rdname doc_handling
#' @export
doc_collection.oafeat <- function(api, req, collection_id) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_collection(db, collection_id)
  doc <- links_collection(doc, api, req)
  doc
}
#' @rdname doc_handling
#' @export
doc_items.oafeat <- function(api,
                             req,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_items(
    db = db,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
  doc <- links_items(
    doc = doc,
    api = api,
    req = req,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
  doc
}
#' @rdname doc_handling
#' @export
doc_item.oafeat <- function(api, req, collection_id, item_id) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  doc <- db_item(db, collection_id, item_id)
  doc <- links_item(doc, api, req)
  doc
}
#' @keywords internal
#' @export
links_landing_page.oafeat <- function(doc, api, req) {
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc <- link_spec(doc, api, req)
  doc <- link_docs(doc, api, req)
  host <- get_host(api, req)
  doc <- update_link(
    doc = doc,
    rel = "conformance",
    href = make_url(host, "/conformance"),
    type = "application/json"
  )
  doc <- update_link(
    doc = doc,
    rel = "data",
    href = make_url(host, "/collections"),
    type = "application/json"
  )
  db <- get_db(api)
  for (collection in db_collections(db)) {
    doc <- add_link(
      doc = doc,
      rel = "child",
      href = make_url(host, "/collections", doc$id),
      type = "application/json",
      title = doc$title
    )
  }
  doc
}
#' @keywords internal
#' @export
links_collections.oafeat <- function(doc, api, req) {
  doc$collections <- lapply(doc$collections, function(collection) {
    links_collection(collection, api, req)
  })
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc
}
#' @keywords internal
#' @export
links_collection.oafeat <- function(doc, api, req) {
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/json")
  doc <- link_parent(doc, api, req)
  host <- get_host(api, req)
  doc <- update_link(
    doc = doc,
    rel = "item",
    href = make_url(host, "/collections", doc$id, "items"),
    type = "application/geo+json"
  )
  doc
}
#' @keywords internal
#' @export
links_items.oafeat <- function(doc,
                               api,
                               req,
                               collection_id,
                               limit,
                               bbox,
                               datetime,
                               page) {
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  host <- get_host(api, req)
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    links_item(item, api, req)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = make_url(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = make_url(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = deparse_array(bbox),
        datetime = deparse_datetime(datetime),
        page = page
      ),
      type = "application/geo+json"
    ),
    new_link(
      rel = "collection",
      href = make_url(host, "/collections", collection_id),
      type = "application/json"
    )
  )
  # add navigation links
  if (page > 1 && page <= pages)
    doc <- add_link(
      doc = doc,
      rel = "prev",
      href = make_url(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = deparse_array(bbox),
        datetime = deparse_datetime(datetime),
        page = page - 1
      ),
      type = "application/geo+json"
    )
  if (page < pages)
    doc <- add_link(
      doc = doc,
      rel = "next",
      href = make_url(
        host = host,
        "/collections",
        collection_id,
        "items",
        limit = limit,
        bbox = deparse_array(bbox),
        datetime = deparse_datetime(datetime),
        page = page + 1
      ),
      type = "application/geo+json"
    )
  doc
}
#' @keywords internal
#' @export
links_item.oafeat <- function(doc, api, req) {
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  host <- get_host(api, req)
  doc <- update_link(
    doc = doc,
    rel = "collection",
    href = make_url(host, "/collections", doc$collection),
    type = "application/json"
  )
  doc
}
