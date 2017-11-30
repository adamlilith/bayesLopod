setClassUnion("spatialObjectsUnion", members = c("RasterStack", "SpatialPolygonsDataFram


#' An S4 class to contain data to be input into a bayesLopod model.
#' @importClassesFrom raster RasterStack RasterLayer
#' @importClassesFrom sp SpatialPolygonsDataFrame
#' @import methods
#' @export LopodData_Class
#' @slot geoDataObject Spatial object supported by bayesLopod
#' @slot geoType Type of geographical data (Only rasters supported for now)
#' @slot geoInfo Additional spatial information to be passed to modelLopod

# geoInfo is list, can be data frame with two columns, one with index referncing geographic unit (eg cell number), other is index for STAN (STAN index can be different from geo object order)

LopodData_Class = setClass("LopodData",contains = "spatialObjectsUnion", slots=c(geoDataObject = "spatialObjectsUnion", geoType = "character", geoInfo = "list" ) )
