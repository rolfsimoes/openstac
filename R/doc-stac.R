#' @rdname doc_handling
#' @export
doc_landing_page.stac <- function(api, req) {
  doc <- list(
    stac_version = api$stac_version,
    type = "Catalog",
    id = api$id,
    title = api$title, description = api$description,
    conformsTo = api$conforms_to
  )
  doc <- links_landing_page(doc, api, get_host(req), get_method(req))
  doc
}
#' @rdname doc_handling
#' @export
doc_conformance.stac <- function(api, req) {
  NextMethod("doc_conformance", api)
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
    host = get_host(req),
    method = get_method(req),
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    ids = ids,
    collections = collections,
    page = page
  )
  doc
}
#' @keywords internal
#' @export
links_landing_page.stac <- function(doc, api, host, method) {
  NextMethod("links_landing_page", api)
}
#' @keywords internal
#' @export
links_collection.stac <- function(doc, api, host, method) {
  NextMethod("links_collection", api)
}
#' @keywords internal
#' @export
links_collections.stac <- function(doc, api, host, method) {
  NextMethod("links_collections", api)
}
#' @keywords internal
#' @export
links_item.stac <- function(doc, api, host, method) {
  NextMethod("links_item", api)
}
#' @keywords internal
#' @export
links_items.stac <- function(doc,
                             api,
                             host,
                             method,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page) {
  NextMethod("links_items", api)
}
#' @keywords internal
#' @export
links_search.stac <- function(doc,
                              api,
                              host,
                              method,
                              limit,
                              bbox,
                              datetime,
                              ids,
                              collections,
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
    )
  )
  # add navigation links
  if (method == "GET") {
    if (page > 1 && page <= pages)
      doc <- add_link(
        doc = doc,
        rel = "prev",
        href = make_url(
          host = host,
          "/search",
          limit = limit,
          bbox = deparse_array(bbox),
          datetime = deparse_datetime(datetime),
          ids = ids,
          collections = collections,
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
          "/search",
          limit = limit,
          bbox = deparse_array(bbox),
          datetime = deparse_datetime(datetime),
          ids = ids,
          collections = collections,
          page = page + 1
        ),
        type = "application/geo+json"
      )
  } else if (method == "POST") {
    doc$links <- list(
      new_link(
        rel = "root",
        href = make_url(host, "/"),
        type = "application/json"
      )
    )
    if (page > 1 && page <= pages)
      doc <- add_link(
        doc = doc,
        rel = "prev",
        href = make_url(host, "/search"),
        body = list(
          page = page - 1
        ),
        merge = TRUE,
        type = "application/geo+json"
      )
    if (page < pages)
      doc <- add_link(
        doc = doc,
        rel = "next",
        href = make_url(host, "/search"),
        body = list(
          page = page + 1
        ),
        merge = TRUE,
        type = "application/geo+json"
      )
  }
  doc
}
