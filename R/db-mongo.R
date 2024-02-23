#' @export
new_db.mongodb <- function(driver, db, url, ..., batch_size = 1000) {
  # driver checkers
  stopifnot(requireNamespace("mongolite"))
  data <- list(
    collections = mongolite::mongo(
      collection = "collections",
      db = db,
      url = url, ...,
    ),
    items = mongolite::mongo(
      collection = "items",
      db = db,
      url = url, ...
    ),
    config = list(
      batch_size = batch_size
    )
  )
  structure(data, class = driver[[1]])
}

#' @export
db_collections_id_exist.mongodb <- function(db, ids) {
  collections_id <- mongo_collections_id(db, ids)
  ids %in% collections_id
}

#' @export
db_collections.mongodb <- function(db) {
  mongo_collections(db)
}

#' @export
db_collection.mongodb <- function(db, collection_id) {
  mongo_collections(db, collection_id = collection_id[[1]])[[1]]
}

#' @export
db_items_id_exist.mongodb <- function(db, collection_id, ids) {
  items_id <- mongo_items_id(db, collection_id, mongo_in("id", ids))
  ids %in% items_id
}

#' @export
db_items.mongodb <- function(db, collection_id, limit, bbox, datetime, page) {
  query <- NULL
  if (!is.null(bbox))
    query <- mongo_and(query, mongo_intersects("geometry", bbox_as_geom(bbox)))
  if (!is.null(datetime))
    query <- mongo_and(query, mongo_filter_datetime(datetime))
  items <- mongo_items(
    db = db,
    collection_id = collection_id,
    query = query,
    limit = limit,
    page = page
  )
  pages <- get_pages(items, limit)
  if (pages > 0 && page > 1) {
    api_stopifnot(
      page <= pages,
      status = 400,
      "page not less than or equal to ", pages
    )
  }
  items
}

#' @export
db_item.mongodb <- function(db, collection_id, item_id) {
  items <- mongo_items(
    db = db,
    collection_id = collection_id,
    query = mongo_equal("id", item_id),
    limit = 1,
    page = 1
  )
  item <- items$features[[1]]
  item$collection <- collection_id
  as_item(item)
}

#' @export
db_search.mongodb <- function(db,
                              limit,
                              bbox,
                              datetime,
                              intersects,
                              ids,
                              collections,
                              page) {
  query <- NULL
  if (!is.null(bbox))
    query <- mongo_and(query, mongo_intersects("geometry", bbox_as_geom(bbox)))
  if (!is.null(datetime))
    query <- mongo_and(query, mongo_filter_datetime(datetime))
  if (!is.null(intersects))
    query <- mongo_and(query, mongo_intersects("geometry", intersects))
  if (!is.null(ids))
    query <- mongo_and(query, mongo_in("id", ids))
  items <- mongo_items(
    db = db,
    collection_id = collections,
    query = query,
    limit = limit,
    page = page
  )
  pages <- get_pages(items, limit)
  if (pages > 0 && page > 1) {
    api_stopifnot(
      page <= pages,
      status = 400,
      "page not less than or equal to ", pages
    )
  }
  items
}

mongo_filter_exact_date <- function(items, exact_date) {

}

mongo_filter_start_date <- function(items, start_date) {

}

mongo_filter_end_date <- function(items, end_date) {

}

mongo_filter_datetime <- function(field, datetime) {
  query <- NULL
  exact_date <- datetime$exact
  start_date <- datetime$start
  end_date <- datetime$end
  # ...exact_date
  if (!is.null(exact_date)) {
    query <- mongo_filter_exact_date(field, exact_date)
  } else {
    # ...start_date
    if (!is.null(start_date))
      query <- mongo_filter_start_date(field, start_date)
    # ...end_date
    if (!is.null(end_date))
      query <- mongo_and(query, mongo_filter_end_date(field, end_date))
  }
  query
}

mongo_collections_id <- function(db, collection_id = NULL, query = NULL) {
  if (!is.null(collection_id))
    query <- mongo_and(query, mongo_in("id", collection_id))
  db$collections$distinct("id", query)
}

mongo_collections <- function(db, collection_id = NULL, query = NULL) {
  if (!is.null(collection_id))
    query <- mongo_and(query, mongo_in("id", collection_id))
  cursor <- db$collections$iterate(
    query = jsonlite::toJSON(query, auto_unbox = TRUE)
  )
  batch <- cursor$batch(db$config$batch_size)
  collections <- list()
  while (!is.null(batch)) {
    collections <- c(collections, batch)
    batch <- possibly(cursor$batch(db$config$batch_size))
  }
  create_collections(collections)
}

mongo_items_id <- function(db, collection_id = NULL, query = NULL) {
  if (!is.null(collection_id))
    query <- mongo_and(query, mongo_in("collection", collection_id))
  db$items$distinct("id", query)
}

mongo_items_matched <- function(db, collection_id = NULL, query = NULL) {
  if (!is.null(collection_id))
    query <- mongo_and(query, mongo_in("collection", collection_id))
  db$items$aggregate(jsonlite::toJSON(
    x = list(
      list(`$match` = query),
      list(`$group` = list(`_id` = 1, `count` = list(`$sum` = 1)))
    ),
    auto_unbox = TRUE
  ))$count
}

mongo_items <- function(db,
                        collection_id = NULL,
                        query = NULL,
                        limit = 10,
                        page = 1) {
  if (!is.null(collection_id))
    query <- mongo_and(query, mongo_in("collection", collection_id))
  cursor <- db$items$iterate(
    query = jsonlite::toJSON(query, auto_unbox = TRUE),
    skip = (page - 1) * limit
  )
  features <- cursor$batch(limit)
  create_items(
    features,
    numberMatched = mongo_items_matched(db, query),
    numberReturned = length(features)
  )
}

# ---- operators ----

mongo_in <- function(field, values) {
  query <- list(list("$in" = as.list(values)))
  names(query) <- field
  query
}
mongo_and <- function(expr1, expr2) {
  if (is.null(expr1)) expr1 <- list()
  utils::modifyList(expr1, expr2)
}
mongo_equal <- function(field, value) {
  query <- list(list("$eq" = value))
  names(query) <- field
  query
}
mongo_geometry <- function(geom) {
  list("$geometry" = geom)
}
mongo_intersects <- function(field, geom) {
  query <- list(list("$geoIntersects" = mongo_geometry(geom)))
  names(query) <- field
  query
}
