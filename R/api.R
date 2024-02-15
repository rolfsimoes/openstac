#' Handle API requests
#'
#' These are low-level functions responsible for handling requests of the
#' API endpoint. It interfaces HTTP requests from `plumber` and uses the
#' `api`, the `req`, and the `res` objects to prepare a response to the
#' request by dispatching to specific API implementations.
#'
#' Users should parse and validate parameters such as `collection_id`,
#' `bbox`, and `datetime` before calling these functions.
#' `openstac` provides functions like `parse_geojson()`, `parse_datetime()`,
#' `parse_dbl()`, `parse_int()`, and `parse_str()` to facilitate this
#' process.
#'
#' \itemize{
#'
#' \item `create_ogcapi`: Creates an API object for OGC API Features.
#'
#' \item `create_stac`: Creates an API object for STAC.
#'
#' \item `api_landing_page`: Handles the STAC `/` endpoint.
#'
#' \item `api_conformance`: Handles the STAC `/conformance` endpoint.
#'
#' \item `api_collections`: Handles the STAC `/collections` endpoint.
#'
#' \item `api_collection`: Handles the STAC
#'   `/collection/{collection_id}` endpoint.
#'
#' \item `api_items`: Handles the STAC
#'   `/collection/{collection_id}/items` endpoint.
#'
#' \item `api_item`: Handles the STAC
#'   `/collection/{collection_id}/item/{item_id}` endpoint.
#'
#' \item `api_search`: Handles the STAC `/search` endpoint.
#'
#' }
#'
#' @param id A character string specifying the id of the API.
#'
#' @param title A character string specifying the title of the API.
#'
#' @param description A character string describing the API.
#'
#' @param conforms_to A character vector specifying the conformance
#'   standards adhered to by the API. This parameter can be NULL or
#'   contain additional conformance standards to add to the defaults.
#'
#' @param api An object representing the API. This object is typically
#'   created using either the `create_stac` or `create_ogcapi`
#'
#' @param req The request object from the `plumber` package, containing
#'   information about the HTTP request made to the API endpoint.
#'
#' @param res The response object from the `plumber` package, used to
#'   construct and send the HTTP response back to the client making
#'   the request.
#'
#' @param collection_id The identifier of the collection. This parameter
#'   specifies which collection the request is targeting.
#'
#' @param item_id The identifier of the item within the specified collection.
#'   This parameter specifies which item the request is targeting.
#'
#' @param limit The maximum number of items to return. If not specified,
#'   the default value is used.
#'
#' @param bbox The bounding box for spatial filtering, specified as a
#'   numeric vector of four coordinates
#'   (`long_min`, `lat_min`, `long_max`, `lat_max`). Use `parse_dbl()` to
#'   convert comma-separated string to numeric vector.
#'
#' @param datetime The temporal filter for items. It must be specified
#'   as a `list(start = start_date, end = end_date, exact = exact_date)`
#'   object. Use `parse_datetime()` function to convert STAC datetime
#'   string to this object.
#'
#' @param intersects The spatial filter for items, specified as a GeoJSON
#'   geometry object representing the area of interest. Use `parse_geojson()`
#'   function to convert strings of GeoJSON geometries into an equivalent
#'   `list()` object.
#'
#' @param ids A list of item identifiers to filter the search results.
#'   Use `parse_str()` to convert a comma-separated string to a
#'   character vector
#'
#' @param collections A list of collection identifiers to filter the
#'   search results. Use `parse_str()` to convert a comma-separated
#'   string to a character vector.
#'
#' @param page The page number of the results when paginating.
#'
#' @param ... Additional arguments to be passed to the method-specific
#'   functions.
#'
#' @return For API creation functions, returns a api object. For API
#'   handling functions, returns the document to return as response.
#'
#' @seealso
#' [create_stac()], [create_ogcapi()]: Functions for creating STAC and
#'   OGC API objects, respectively.
#'
#' [parse_int()], [parse_dbl()], [parse_str()], [parse_datetime()],
#'   [parse_geojson()]: Functions to convert HTTP input strings
#'   into R data types.
#'
#' @references
#' For more information about the STAC specification,
#' see: \url{https://stacspec.org/}
#'
#' For more information about the OGC API specification,
#' see: \url{http://www.opengis.net/doc/IS/ogcapi-features-1/1.0}
#'
#' @name api_handling
#'
NULL
#' @keywords internal
create_api <- function(api_class,
                       title,
                       description,
                       conforms_to, ...) {
  structure(
    list(
      title = title,
      description = description,
      conforms_to = conforms_to, ...
    ),
    class = api_class,
    env = new.env(hash = TRUE, parent = parent.frame())
  )
}
#' @rdname api_handling
#' @export
create_stac <- function(id,
                        title,
                        description,
                        conforms_to = NULL, ...) {
  create_api(
    api_class = c("stac", "ogcapi"),
    title = title,
    description = description,
    conforms_to = conforms_to,
    stac_version = "1.0.0",
    id = id, ...
  )
}
#' @rdname api_handling
#' @export
create_ogcapi <- function(title,
                          description,
                          conforms_to = NULL, ...) {
  # A list of all conformance classes specified in a standard that the
  # server conforms to.
  ogcapi_conforms_to <- c(
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
    "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
  )
  create_api(
    api_class = "ogcapi",
    title = title,
    description = description,
    conforms_to = c(ogcapi_conforms_to, conforms_to), ...
  )
}
#' @keywords internal
api_env <- function(api) {
  attr(api, "env")
}
#' @keywords internal
api_attr <- function(api, name) {
  if (exists(name, envir = api_env(api)))
    get(name, envir = api_env(api), inherits = FALSE)
}
#' @keywords internal
`api_attr<-` <- function(api, name, value) {
  assign(name, value, envir = api_env(api), inherits = FALSE)
  api
}
#' @rdname api_handling
#' @export
api_setup <- function(api, pr, handle_errors = TRUE) {
  api_attr(api, "plumber") <- pr
  plumber::pr_set_docs(pr, FALSE)
  if (handle_errors)
    plumber::pr_set_error(pr, api_error_handler)
  api
}
#' @keywords internal
get_plumber <- function(api) {
  pr <- api_attr(api, "plumber")
  if (is.null(pr))
    api_stop(
      status = 500,
      "The API was not setup properly. Please, use ",)
}
#' @rdname api_handling
#' @export
api_landing_page <- function(api, req, res) {
  doc_landing_page(api, req)
}
#' @rdname api_handling
#' @export
api_conformance <- function(api, req, res) {
  doc_conformance(api, req)
}
#' @rdname api_handling
#' @export
api_collections <- function(api, req, res) {
  doc_collections(api, req)
}
#' @rdname api_handling
#' @export
api_collection <- function(api, req, res, collection_id) {
  doc_collection(api, req, collection_id)
}
#' @rdname api_handling
#' @export
api_items <- function(api,
                      req,
                      res,
                      collection_id,
                      limit,
                      bbox,
                      datetime,
                      page) {
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
  doc_items(
    api = api,
    req = req,
    collection_id = collection_id,
    limit = limit,
    bbox = bbox,
    datetime = datetime,
    page = page
  )
}
#' @rdname api_handling
#' @export
api_item <- function(api, req, res, collection_id, item_id) {
  doc_item(api, req, collection_id, item_id)
}
#' @rdname api_handling
#' @export
api_search <- function(api,
                       req,
                       res,
                       limit,
                       bbox,
                       datetime,
                       intersects,
                       ids,
                       collections,
                       page) {
  # check parameters
  if (!is.null(limit)) {
    limit <- parse_int(limit[[1]])
    check_limit(limit, min = 1, max = 10000)
  }
  if (missing(bbox)) bbox <- NULL
  if (missing(intersects)) intersects <- NULL
  api_stopifnot(
    is.null(bbox) || is.null(intersects),
    status = 400,
    "only one of either intersects or bbox may be provided"
  )
  if (!is.null(bbox)) {
    bbox <- parse_dbl(bbox)
    check_bbox(bbox)
  }
  if (missing(datetime)) datetime <- NULL
  if (!is.null(datetime)) {
    datetime <- parse_datetime(datetime[[1]])
  }
  method <- get_method(req)
  if (!is.null(intersects)) {
    api_stopifnot(
      method == "POST",
      status = 405,
      "the request method is not supported"
    )
    intersects <- parse_geojson(intersects)
    check_intersects(intersects)
  }
  if (missing(ids)) ids <- NULL
  if (!is.null(ids)) ids <- parse_str(ids)
  api_stopifnot(
    !missing(collections),
    status = 400,
    "collections parameter must be provided"
  )
  if (!is.null(collections)) {
    collections <- parse_str(collections)
    check_collections(collections)
  }
  if (!is.null(page)) {
    page <- parse_int(page[[1]])
    check_page(page)
  }
  doc_search(
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
}
api_docs <- function(pr) {
  list(
    name = "swagger",
    index = function(version = "3", ...) {
      swagger::swagger_spec(
        api_path = paste0(
          "window.location.origin + ",
          "window.location.pathname.replace(", "/\\(", "__swagger__\\\\/|",
          "__swagger__\\\\/", "index.html|", "__docs__\\\\/|",
          "__docs__\\\\/", "index.html", "\\)$/, ", "\"\"",
          ") + \"openapi.json\""),
        version = version)
    }, static = function(version = "3", ...) {
      swagger::swagger_path(version)
  })
}
api_spec <- function(api, req) {
  pr <- get_plumber(api)
  spec <- pr$getApiSpec()
  utils::modifyList(
    list(servers = list(list(
      url = make_url(get_host(req))
    ))),
    spec
  )
}
