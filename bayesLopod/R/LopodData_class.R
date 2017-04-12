#' An S4 class to contain data to be input into a bayesLopod model.
#'
#' @slot geoDataObject Spatial object with layers 'studyArea', 'sampligEffort', 'Detections'
#' @slot geoType Type of geographical data (Only rasters supported for now)
#' @slot geoInfo Additional spatal information to be passsed to modelLopod

LopodData_Class = setClass("LopodData",slots=c(geoDataObject = "RasterStack", geoType = "character", geoInfo = "list" ) )
