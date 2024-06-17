#' @rdname api_handling
#' @export
api_landing_page.oafeat <- function(api, ...) {
  req <- get_req(...)
  host <- get_host(api, req)
  doc <- list(title = api$title(), description = api$description()) |>
    link_root(api, req) |>
    link_self(api, req, "application/json") |>
    link_spec(api, req) |>
    link_docs(api, req) |>
    update_link(
      rel = "conformance",
      href = make_url(host, "/conformance"),
      type = "application/json"
    ) |>
    update_link(
      rel = "data",
      href = make_url(host, "/collections"),
      type = "application/json"
    )
  db <- get_db(api)
  doc$links <- c(doc$links, lapply(db_collections(db), \(col) {
    new_link(
      rel = "child",
      href = make_url(host, "/collections", escape_url(col$id)),
      type = "application/json",
      title = col$title
    )
  }))
  doc
}
#' @rdname api_handling
#' @export
api_conformance.oafeat <- function(api, ...) {
  doc <- list(conformsTo = api$conforms_to())
  doc
}
#' @rdname api_handling
#' @export
api_collections.oafeat <- function(api, ...) {
  # TODO: implement pagination limit
  req <- get_req(...)
  host <- get_host(api, req)
  db <- get_db(api)
  doc <- create_collections(db_collections(db)) |>
    map_collections(\(col) {
      col <- col |>
        link_root(api, req) |>
        link_self(api, req, "application/json") |>
        link_parent(api, req) |>
        update_link(
          rel = "item",
          href = make_url(host, "/collections", escape_url(col$id), "items"),
          type = "application/geo+json"
        )
      col
    }) |>
    link_root(api, req) |>
    link_self(api, req, "application/json")
  doc
}
#' @rdname api_handling
#' @export
api_collection.oafeat <- function(api, collection_id, ...) {
  req <- get_req(...)
  host <- get_host(api, req)
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  doc <- db_collection(db, collection_id) |>
    link_root(api, req) |>
    link_self(api, req, "application/json") |>
    link_parent(api, req) |>
    update_link(
      rel = "item",
      href = make_url(host, "/collections", escape_url(collection_id), "items"),
      type = "application/geo+json"
    )
  doc
}
#' @rdname api_handling
#' @export
api_items.oafeat <- function(api,
                             collection_id,
                             limit,
                             bbox,
                             datetime,
                             page, ...) {
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  # do items
  req <- get_req(...)
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
  doc <- link_root(doc, api, req)
  doc <- link_self(doc, api, req, "application/geo+json")
  host <- get_host(api, req)
  doc <- doc |>
    map_features(\(item) {
      item <- item |>
        link_root(api, req) |>
        link_self(api, req, "application/geo+json") |>
        update_link(
          rel = "collection",
          href = make_url(host, "/collections", item$collection),
          type = "application/json"
        )
      item
    }) |>
    add_link(
      rel = "root",
      href = make_url(host, "/"),
      type = "application/json"
    ) |>
    add_link(
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
    ) |>
    add_link(
      rel = "collection",
      href = make_url(host, "/collections", collection_id),
      type = "application/json"
    ) |>
    links_navigation(
      doc = doc,
      api = api,
      req = req,
      "/collections",
      collection_id,
      "items",
      bbox = deparse_array(bbox),
      datetime = deparse_datetime(datetime),
      limit = limit,
      page = page,
      type = "application/geo+json"
    )
  doc
}
#' @rdname api_handling
#' @export
api_item.oafeat <- function(api, collection_id, item_id, ...) {
  req <- get_req(...)
  host <- get_host(api, req)
  db <- get_db(api)
  check_collection_in_db(db, collection_id)
  check_item_in_db(db, collection_id, item_id)
  doc <- db_item(db, collection_id, item_id) |>
    link_root(api, req) |>
    link_self(api, req, "application/geo+json") |>
    update_link(
      rel = "collection",
      href = make_url(host, "/collections", collection_id),
      type = "application/json"
    )
  doc
}
