#' An S4 class to contain data to be input into a bayesLopod model.
#'
#' @slot geoDataObject Spatial object supported by bayesLopod
#' @slot geoType Type of geographical data (Only rasters supported for now)
#' @slot geoInfo Additional spatal information to be passsed to modelLopod

setClassUnion("spatialObjectsUnion", members = c("RasterStack", "SpatialPolygonsDataFrame"))
LopodData_Class = setClass("LopodData",contains = "spatialObjectsUnion", slots=c(geoDataObject = "spatialObjectsUnion", geoType = "character", geoInfo = "list" ) )
