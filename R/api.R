#' Handle API requests
#'
#' These are functions responsible for handling requests of the
#' API endpoint. It interfaces HTTP requests from `plumber` and uses the
#' `api` and the `req` objects to prepare a response to the request by
#' dispatching to specific API implementations. HTTP input parameters are
#' parsed internally.
#'
#' \itemize{
#'
#' \item `create_api`: Creates an API object. It allows users setup
#'   custom API classes to create response documents.
#'
#' \item `create_oafeat`: Creates an API object for OGC API Features.
#'
#' \item `create_stac`: Creates an API object for STAC.
#'
#' \item `setup_plumber`: Register the Plumber router in the API server.
#'   It also can enable the Plumber documentation and set the handler
#'   of errors in the API.
#'
#' \item `api_spec`: Generates the OpenAPI specification for the API server.
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
#' @param api_class A character string specifying the custom S3 class
#'   of the API. It allows advanced users setup new classes to handle
#'   response documents. Currently, `openstac` supports `oafeat` and
#'   `stac` S3 classes. To implement a new set of response document
#'   handlers, users must implement for their new class all generic
#'   functions declared in `R/doc.R`. For more details, see the
#'   `github` page of the project.
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
#' @param pr The Plumber router object to be associated with the API server.
#'   For annotated API definition, users can capture the current Plumber
#'   object by annotating `@plumber` keyword in comment block. See
#'   references below for more details.
#'
#' @param spec_endpoint The endpoint where the API specification
#'   (OpenAPI) will be available. An `NULL` value disable this feature.
#'
#' @param docs_endpoint The endpoint where the API documentation
#'   (swagger) will be available. An `NULL` value disable this feature.
#'
#' @param handle_errors A logical value indicating whether to handle
#'   errors using the `openstac` default error handler. Default is `TRUE`.
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
#'   comma-separated string of four coordinates
#'   (`long_min`,`lat_min`,`long_max`,`lat_max`).
#'
#' @param datetime The temporal filter for items. It must be specified
#'   as a STAC datetime interval or timestamp string
#'   (e.g. `2020-03-29/2021-12-31`).
#'
#' @param intersects The spatial filter for items, specified as a GeoJSON
#'   geometry object representing the area of interest. The data comes
#'   as string representing a GeoJSON geometry.
#'
#' @param ids A comma-separated string of item identifiers to filter
#'   the search results.
#'
#' @param collections A comma-separated string of collection identifiers
#'   to filter the search results.
#'
#' @param page The page number of the results when paginating.
#'
#' @param ... Additional arguments to be passed to the method-specific
#'   functions.
#'
#' @return For API creation functions, returns a api object. For API
#'   handling functions, returns the document to return as response.
#'
#' @references
#' For more information about the STAC specification,
#' see: \url{https://stacspec.org/}
#'
#' For more information about the OGC API specification,
#' see: \url{http://www.opengis.net/doc/IS/ogcapi-features-1/1.0}
#'
#' For more information about annotated Plumber API definition, see:
#' \url{https://www.rplumber.io/articles/annotations.html}
#'
#' @name api_handling
#'
NULL
#' @rdname api_handling
#' @export
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
    api_class = c("stac", "oafeat"),
    title = title,
    description = description,
    conforms_to = conforms_to,
    stac_version = "1.0.0",
    id = id, ...
  )
}
#' @rdname api_handling
#' @export
create_oafeat <- function(title,
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
    api_class = "oafeat",
    title = title,
    description = description,
    conforms_to = c(ogcapi_conforms_to, conforms_to), ...
  )
}
#' @rdname api_handling
#' @export
setup_plumber <- function(api,
                          pr,
                          spec_endpoint = "/api",
                          docs_endpoint = "/docs",
                          handle_errors = TRUE) {
  api_attr(api, "plumber") <- pr
  if (!is.null(spec_endpoint)) {
    plumber_setup_spec(pr, spec_endpoint)
    if (!is.null(docs_endpoint))
      plumber_setup_docs(pr, docs_endpoint, spec_endpoint)
  }
  if (handle_errors)
    plumber::pr_set_error(pr, api_error_handler)
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
