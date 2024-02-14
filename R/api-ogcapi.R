# A list of all conformance classes specified in a standard that the
# server conforms to.
ogcapi_conforms_to <- c(
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)
#' @rdname api_handling
#' @export
create_ogcapi <- function(title, description, conforms_to = NULL, ...) {
  create_api(
    api_class = "ogcapi",
    title = title,
    description = description,
    conforms_to = c(ogcapi_conforms_to, conforms_to), ...
  )
}
#' @rdname api_handling
#' @export
api_landing_page.ogcapi <- function(api, req, res, ...) {
  list(title = api$title, description = api$description) |>
    links_landing_page(api, req, res)
}
#' @rdname api_handling
#' @export
api_conformance.ogcapi <- function(api, req, res, ...) {
  list(conformsTo = api$conforms_to)
}
#' @rdname api_handling
#' @export
api_collections.ogcapi <- function(api, req, res, ...) {
  db <- get_db(api)
  list(collections = db_collections(db)) |>
    links_collections(api, req, res)
}
#' @rdname api_handling
#' @export
api_collection.ogcapi <- function(api, req, res, collection_id, ...) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  db_collection(db, collection_id) |>
    links_collection(api, req, res)
}
#' @rdname api_handling
#' @export
api_items.ogcapi <- function(api,
                             req,
                             res,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page, ...) {
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
  links_items(
    doc = doc,
    api = api,
    req = req,
    res = res,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}
#' @rdname api_handling
#' @export
api_item.ogcapi <- function(api, req, res, collection_id, item_id, ...) {
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  db_item(db, collection_id, item_id) |>
    links_item(api, req, res)
}
#' @export
links_landing_page.ogcapi <- function(doc, api, req, res, ...) {
  host <- get_host(req)
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
#' @export
links_collection.ogcapi <- function(doc, api, req, res, ...) {
  host <- get_host(req)
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
#' @export
links_collections.ogcapi <- function(doc, api, req, res, ...) {
  doc$collections <- lapply(doc$collections, function(collection) {
    links_collection(collection, api, req, res)
  })
  host <- get_host(req)
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
#' @export
links_item.ogcapi <- function(doc, api, req, res, ...) {
  host <- get_host(req)
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
#' @export
links_items.ogcapi <- function(doc,
                                api,
                                req,
                                res,
                                collection_id,
                                limit,
                                bbox,
                                datetime,
                                page, ...) {
  pages <- get_pages(doc, limit)
  # update item links
  doc$features <- lapply(doc$features, function(item) {
    links_item(item, api, req, res)
  })
  host <- get_host(req)
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
        bbox = bbox,
        datetime = datetime_as_str(datetime),
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
        bbox = bbox,
        datetime = datetime_as_str(datetime),
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
        bbox = bbox,
        datetime = datetime_as_str(datetime),
        page = page + 1
      ),
      type = "application/geo+json"
    )
  doc
}
