#* @apiTitle OpenLandMap STAC API Example
#* @apiDescription Example of Spatio-Temporal Asset Catalog for global
#*   layers provided by OpenLandMap and maintaned by OpenGeoHub Foundation
#* @apiVersion 1.0.0
#* @apiBasePath /

# Load libraries
library(openstac)

# A list of all conformance classes specified in a standard that the
# server conforms to.
conforms_to <- c(
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/core",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/oas30",
  "http://www.opengis.net/spec/ogcapi-features-1/1.0/conf/geojson"
)

# Create an STAC server API object
api <- create_stac(
  id = "openlandmap",
  title = "OpenLandMap STAC API Example",
  description = paste("Example of Spatio-Temporal Asset Catalog for",
                      "global layers provided by OpenLandMap and",
                      "maintaned by OpenGeoHub Foundation"),
  conforms_to = conforms_to
)

# Set API database
db_file <- system.file("db/olm-example.rds", package = "openstac")
api <- set_db(api, driver = "local", file = db_file)

#* Setup plumber router
#* @plumber
function(pr) {
  setup_plumber(api, pr, spec_endpoint = "/api", docs_endpoint = "/docs")
}

#* Landing page
#* @get /
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_landing_page(api, req, res)
}

#* Conformance endpoint
#* @get /conformance
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_conformance(api, req, res)
}

#* Collections endpoint
#* @get /collections
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res) {
  api_collections(api, req, res)
}

#* Collection endpoint
#* @get /collections/<collection_id>
#* @param collection_id:str The ID of the collection
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id) {
  api_collection(api, req, res, collection_id)
}

#* Items endpoint
#* @get /collections/<collection_id>/items
#* @param collection_id:str The ID of the collection
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box in OGC:CRS84 (long_min,lat_min,long_max,lat_max)
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
  api_items(
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

#* Item endpoint
#* @get /collections/<collection_id>/items/<item_id>
#* @param collection_id:str The ID of the collection
#* @param item_id:str The ID of the item
#* @serializer unboxedJSON
#* @tag 'STAC API v1.0.0'
function(req, res, collection_id, item_id) {
  api_item(api, req, res, collection_id, item_id)
}

#* Search endpoint
#* @get /search
#* @param limit:int Maximum number of features to return (default: 10)
#* @param bbox:str Bounding box in OGC:CRS84 (long_min,lat_min,long_max,lat_max)
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
  api_search(
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
}
