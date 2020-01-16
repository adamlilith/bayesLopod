setClassUnion('spatialObjectsUnion', members = c('RasterStack', 'SpatialPolygonsDataFrame'))

#' An S4 class to contain data to be input into a bayesLopod model.
#' @importClassesFrom raster RasterStack RasterLayer
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @import methods
#' @export LopodData_Class
#' @slot geoDataObject Spatial object supported by bayesLopod
#' @slot geoType Type of geographical data (Only rasters supported for now)
#' @slot geoInfo Additional spatial information to be passed to modelLopod

LopodData_Class <- setClass(
	'LopodData',
	contains = 'spatialObjectsUnion',
	slots=c(
		geoDataObject = 'spatialObjectsUnion',
		geoType = 'character',
		geoInfo = 'list'
	)
)
