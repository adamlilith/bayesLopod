#' Create a lopodData object from rasters, a spatial object, or a data frame
#'
#' @param x Either a \code{RasterStack} or \code{RasterBrick} object, a \code{SpatialPolygonsDataFrame} or \code{SpatialLinesDataFrame} object, or a data frame. This function should be used to create the data object provided to the \code{trainLopod} function.
#' @param detect Indicates the field or raster with values representing detections of the target organism. Depending on the class of \code{x}:
#' \list{
#'		\item \code{x} is a data frame, \code{SpatialPolygonsDataFrame}, or \code{SpatialLinesDataFrame}: Either the name of the field (column) or an integer indicating which column represents detections. Default is to use the first column.
#'		\item \code{x} is a \code{RasterStack} or \code{RasterBrick}: Either the name of the raster or an integer indicating which raster represents detections. Default is to use the first raster.
#' }
#' Values represented in the column/raster must be integers >= 0 or \code{NA}. 
#' @param effort Indicates the field or raster with values representing effort of the target organism, depending on the class of \code{x}:
#' \list{
#'		\item \code{x} is a data frame, \code{SpatialPolygonsDataFrame}, or \code{SpatialLinesDataFrame}: Either the name of the field (column) or an integer indicating which column represents effort. Default is to use the second column.
#'		\item \code{x} is a \code{RasterStack} or \code{RasterBrick}: Either the name of the raster or an integer indicating which raster represents effort.  Default is to use the second raster.
#' }
#' Values represented in the column/raster must be integers >= 0 or \code{NA} and must be >= the corresponding values in \code{detect}. 
#' @param @makeAdj Logical, if \code{TRUE} (default) and \code{x} is not "just" a data frame, the calculate an adjacency matrix to allow the \code{trainLopod} function to use conditional autoregression (CAR). This argument is ignored if \code{x} is a data frame. Note that cells/features with no neighbors will be dropped from the analysis. If \code{x} is a raster-type object, please see the help for \code{\link[raster]{adjacent}} for options you may want to use.
#' @param keep Logical, if \code{TRUE} (default) then the lopodData object will retain a copy of the object in \code{x}.
#' @param ... Additional arguments to pass to the function \code{adjacent} used by the \pck{raster} package if \code{x} is a raster.
#' @return
#' An list object that is also of class \code{lopodData}.
#' @examples
#' data(andropogon)
#" data <- makeLopodData(andropogon, 'detections', 'effort', makeAdj=TRUE)
#' str(data, 2)
#' @export

makeLopodData <- function(
	x,
	detect = 1L,
	effort = 2L,
	makeAdj = TRUE,
	keep = TRUE,
	...
) {

	if (!makeAdj) adj <- NA

	xClass <- class(x)
	
	# spatial polygons object
	if (xClass == 'data.frame') {
		
		detects <- x[ , detect]
		efforts <- x[ , effort]

	# spatial polygons object
	} else if (xClass %in% c('SpatialPolygonsDataFrame', 'SpatialLinesDataFrame')) {
		
		detects <- x@data[ , detect]
		efforts <- x@data[ , effort]

		# adjacency matrix
		if (makeAdj) {
			
			adj <- matrix(0, nrow=length(x), ncol=length(x))
			rownames(x@data) <- 1:nrow(x@data)
			neighs <- rgeos::gTouches(x, byid=TRUE, returnDense=FALSE)
			
			if (length(neighs) == 1) {
				stop('There are no neighboring features in "x", so all features would be dropped from the analysis.')
			} else {
				for (i in seq_along(neighs)) {
					adj[i, neighs[[i]]] <- 1
					adj[neighs[[i]], i] <- 1
				}
			}
			
		}
	
	} else if (xClass %in% c('RasterStack', 'RasterBrick')) {
		
		detects <- x[[detect]]
		efforts <- x[[effort]]
		
		notNa <- detects + efforts
		notNaId <- which(!is.na(c(matrix(notNa))))
		
		adj <- raster::adjacent(notNa, cells=notNaId, ...)
		
	} else {
	
		stop('Object "x" must be a data frame, SpatialPolygonsDataFrame, SpatialLinesDataFrame, RasterStack, or RasterBrick object.')
		
	}
	
	out <- list()
	if (keep) out$orig <- x
	out$adj <- adj
	out$data <- data.frame(detects = detects, efforts = efforts)

	class(out) <- c('lopoData', class(out))
	out

}
