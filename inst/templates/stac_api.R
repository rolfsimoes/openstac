#* @apiTitle {{title}}
#* @apiDescription {{description}}
#* @apiVersion 1.0.0
#* @apiBasePath /

library(openstac)
library(plumber)
library(promises)
library(future)

future::plan(future::multisession(workers = {{workers}}))

conforms_to <- c(
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson",
  "https://api.stacspec.org/v1.0.0/core",
  "https://api.stacspec.org/v1.0.0/collections",
  "https://api.stacspec.org/v1.0.0/item-search",
  "https://api.stacspec.org/v1.0.0/ogcapi-features"
)

api <- openstac::create_stac(
  id = "{{id}}",
  title = "{{title}}",
  description = "{{description}}",
  conforms_to = conforms_to
)

api <- openstac::set_db(api, driver = "local", file = "{{db_file}}")

#* @plumber
function(pr) {
  openstac::setup_plumber(
    api = api,
    pr = pr,
    handle_errors = TRUE,
    api_base_url = "{{api_base_url}}",
    spec_endpoint = "{{spec_endpoint}}",
    docs_endpoint = "{{docs_endpoint}}"
  )
}

#* Enable Cross-origin Resource Sharing
#* @filter cors
function(req, res) {
  openstac::api_cors_handler(req, res, origin = "*", methods = "*")
}

#* Landing page
#* @get /
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  openstac::api_landing_page(api, req, res)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  openstac::api_conformance(api, req, res)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  openstac::api_collections(api, req, res)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id) {
  openstac::api_collection(api, req, res, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req,
         res,
         collection_id,
         limit = 10,
         bbox,
         datetime,
         page = 1) {
  # Evaluate before calling promises
  if (missing(bbox)) {
    bbox <- NULL
  }
  if (missing(datetime)) {
    datetime <- NULL
  }
  # call api items in asynchronously
  promises::future_promise({
    openstac::api_items(
      api = api,
      req = req,
      res = res,
      collection_id = collection_id,
      limit = limit,
      bbox = bbox,
      datetime = datetime,
      page = page
    )
  })
}

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id, item_id) {
  openstac::api_item(api, req, res, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box (minx,miny,maxx,maxy)
#* @param datetime:str Datetime filter
#* @param intersects:str GeoJSON geometry to do spatial search
#* @param ids:str Array of items ID to return
#* @param collections:str Array of collection ID
#* @param page:int Pagination parameter (default: 1)
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req,
         res,
         limit = 10,
         bbox,
         datetime,
         intersects,
         ids,
         collections,
         page = 1) {
  # Evaluate before calling promises
  if (missing(bbox)) {
    bbox <- NULL
  }
  if (missing(datetime)) {
    datetime <- NULL
  }
  if (missing(intersects)) {
    intersects <- NULL
  }
  if (missing(ids)) {
    ids <- NULL
  }
  if (missing(collections)) {
    collections <- NULL
  }
  # call api search asynchronously
  promises::future_promise({
    openstac::api_search(
      api = api,
      req = req,
      res = res,
      limit = limit,
      bbox = bbox,
      datetime = datetime,
      intersects = intersects,
      ids = ids,
      collections = collections,
      page = page
    )
  })
}

#* Search endpoint
#* @post /search
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  limit <- req$argsBody$limit
  bbox <- req$argsBody$bbox
  datetime <- req$argsBody$datetime
  intersects <- req$argsBody$intersects
  ids <- req$argsBody$ids
  collections <- req$argsBody$collections
  page <- req$argsBody$page

  # call api search asynchronously
  promises::future_promise({
    openstac::api_search(
      api = api,
      req = req,
      res = res,
      limit = limit,
      bbox = bbox,
      datetime = datetime,
      intersects = intersects,
      ids = ids,
      collections = collections,
      page = page
    )
  })
}
