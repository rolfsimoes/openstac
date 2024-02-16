#' @rdname doc_handling
#' @export
doc_landing_page.oafeat <- function(api, req) {
  doc <- list(title = api$title, description = api$description)
  doc <- links_landing_page(doc, api, get_host(req), get_method(req))
  doc
}
#' @rdname doc_handling
#' @export
doc_conformance.oafeat <- function(api, req) {
  doc <- list(conformsTo = api$conforms_to)
  doc
}
#' @rdname doc_handling
#' @export
doc_collections.oafeat <- function(api, req) {
  db <- get_db(api)
  doc <- list(collections = db_collections(db))
  doc <- links_collections(doc, api, get_host(req), get_method(req))
  doc
}
#' @rdname doc_handling
#' @export
doc_collection.oafeat <- function(api, req, collection_id) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_collection(db, collection_id)
  doc <- links_collection(doc, api, get_host(req), get_method(req))
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
    host = get_host(req),
    method = get_method(req),
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
  doc <- links_item(doc, api, get_host(req), get_method(req))
  doc
}
#' @keywords internal
#' @export
links_landing_page.oafeat <- function(doc, api, host, method) {
  doc$links  <- list(
    new_link(
      rel = "root",
      href = make_url(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = make_url(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "conformance",
      href = make_url(host, "/conformance"),
      type = "application/json"
    ),
    new_link(
      rel = "data",
      href = make_url(host, "/collections"),
      type = "application/json"
    ),
    new_link(
      rel = "search",
      href = make_url(host, "/search"),
      type = "application/geo+json",
      title = "STAC search",
      method = "GET"
    ),
    new_link(
      rel = "search",
      href = make_url(host, "/search"),
      type = "application/geo+json",
      title = "STAC search",
      method = "POST"
    ),
    new_link(
      rel = "service-doc",
      href = make_url(host, "/__docs__/"),
      type = "text/html",
      title = "the API documentation"
    ),
    new_link(
      rel = "service-spec",
      href = make_url(host, "/openapi.json"),
      type = "application/vnd.oai.openapi+json;version=3.0",
      title = "API conformance classes implemented by this server"
    )
  )
  db <- get_db(api)
  doc$links <- c(
    doc$links,
    lapply(db_collections(db), function(doc) {
      new_link(
        rel = "child",
        href = make_url(host, "/collections", doc$id),
        type = "application/json",
        title = doc$title
      )
    })
  )
  doc
}
#' @keywords internal
#' @export
links_collection.oafeat <- function(doc, api, host, method) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = make_url(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = make_url(host, "/collections", doc$id),
      type = "application/json"
    ),
    new_link(
      rel = "item",
      href = make_url(host, "/collections", doc$id, "items"),
      type = "application/geo+json"
    )
  )
  doc
}
#' @keywords internal
#' @export
links_collections.oafeat <- function(doc, api, host, method) {
  doc$collections <- lapply(doc$collections, function(collection) {
    links_collection(collection, api, host, method)
  })
  doc$links <- list(
    new_link(
      rel = "root",
      href = make_url(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = make_url(host, "/collections"),
      type = "application/json"
    )
  )
  doc
}
#' @keywords internal
#' @export
links_item.oafeat <- function(doc, api, host, method) {
  doc$links <- list(
    new_link(
      rel = "root",
      href = make_url(host, "/"),
      type = "application/json"
    ),
    new_link(
      rel = "self",
      href = make_url(host, "/collections", doc$collection,
                      "items", doc$id),
      type = "application/geo+json"
    ),
    new_link(
      rel = "collection",
      href = make_url(host, "/collections", doc$collection),
      type = "application/json"
    )
  )
  doc
}
#' @keywords internal
#' @export
links_items.oafeat <- function(doc,
                               api,
                               host,
                               method,
                               collection_id,
                               limit,
                               bbox,
                               datetime,
                               page) {
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    links_item(item, api, host, method)
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
