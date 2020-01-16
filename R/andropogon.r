#' Spatial data: Number of detection of the dominant grass Andropogon gerardii and it's target group in several counties in the Midwestern US
#'
#' Detections of Andropogon gerardii.
#'
#' @docType data
#'
#' @usage data(andropogon)
#'
#' @format An object of class \code{'SpatialPolygonsDataFrame'}. See \code{\link[sp]{SpatialPolygons}}.
#'
#' @keywords datasets
#'
#' @source Smith, A.B., Alsdurf, J., Knapp, M. and Johnson, L.C. 2017. Phenotypic distribution models corroborate species distribution models: A shift in the role and prevalence of a dominant prairie grass in response to climate change. Global Change Biology 23:4365-4375.
#'
#' @examples
#' data(andropogon)
#' plot(andropogon)
#' head(andropogon@data)
#' # detections of Andropogon gerardii and of any Poaceae
#' grays <- paste0('gray', 0:100)
#' par(mfrow=c(1, 2))
#' detectionsScale <- round(100 * andropogon$detections / max(andropogon$detections))
#' detectionsCol <- grays[detectionsScale]
#' effortScale <- round(100 * andropogon$effort / max(andropogon$effort))
#' effortCol <- grays[effortScale]
#' plot(andropogon, col=detectionsCol, main='Andropogon detections\n(lighter = more)')
#' plot(andropogon, col=effortCol, main='Any Poaceae detections')
'andropogon'
