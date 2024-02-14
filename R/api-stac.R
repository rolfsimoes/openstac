# STAC API version implemented by this S3 class
stac_version <- "1.0.0"
#' @rdname api_handling
#' @export
create_stac <- function(id, title, description, conforms_to = NULL, ...) {
  create_api(
    api_class = c("stac", "ogcapi"),
    title = title,
    description = description,
    conforms_to = conforms_to,
    stac_version = stac_version,
    id = id, ...
  )
}
#' @rdname api_handling
#' @export
api_landing_page.stac <- function(api, req, res, ...) {
  doc <- list(
    stac_version = api$stac_version,
    type = "Catalog",
    id = api$id,
    title = api$title, description = api$description,
    conformsTo = api$conforms_to
  )
  doc <- links_landing_page(doc, api, req, res)
  doc
}
#' @rdname api_handling
#' @export
api_conformance.stac <- function(api, req, res, ...) {
  NextMethod("api_conformance", api)
}
#' @rdname api_handling
#' @export
api_collections.stac <- function(api, req, res, ...) {
  NextMethod("api_collections", api)
}
#' @rdname api_handling
#' @export
api_collection.stac <- function(api, req, res, collection_id, ...) {
  NextMethod("api_collection", api)
}
#' @rdname api_handling
#' @export
api_items.stac <- function(api,
                           req,
                           res,
                           collection_id,
                           limit,
                           bbox,
                           datetime,
                           page, ...) {
  NextMethod("api_items", api)
}
#' @rdname api_handling
#' @export
api_item.stac <- function(api, req, res, collection_id, item_id, ...) {
  NextMethod("api_item", api)
}
#' @rdname api_handling
#' @export
api_search.stac <- function(api,
                            req,
                            res,
                            limit,
                            bbox,
                            datetime,
                            intersects,
                            ids,
                            collections,
                            page, ...) {
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
  links_search(
    doc = doc,
    api = api,
    req = req,
    res = res,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    ids = ids,
    collections = collections,
    page = page
  )
}
#' @export
links_landing_page.stac <- function(doc, api, req, res, ...) {
  NextMethod("links_landing_page", api)
}
#' @export
links_collection.stac <- function(doc, api, req, res, ...) {
  NextMethod("links_collection", api)
}
#' @export
links_collections.stac <- function(doc, api, req, res, ...) {
  NextMethod("links_collections", api)
}
#' @export
links_item.stac <- function(doc, api, req, res, ...) {
  NextMethod("links_item", api)
}
#' @export
links_items.stac <- function(doc,
                             api,
                             req,
                             res,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page, ...) {
  NextMethod("links_items", api)
}
#' @export
links_search.stac <- function(doc,
                              api,
                              req,
                              res,
                              limit,
                              bbox,
                              datetime,
                              ids,
                              collections,
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
    )
  )
  method <- get_method(req)
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
          bbox = bbox,
          datetime = datetime_as_str(datetime),
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
          bbox = bbox,
          datetime = datetime_as_str(datetime),
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
