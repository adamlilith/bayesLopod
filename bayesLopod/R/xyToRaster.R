#' Crate Shape obejct for a parameter estimated in a LopodModel
#'
#' @param xyRecords Object of class SpatialPoints or SpatialPointsDataFrame with the locality-records of the species.
#' @param xySamplingEffort Object of class SpatialPoints or SpatialPointsDataFrame with the coordinates of all sampling events (including those in which the species was found).
#' @param nrows Number of rows that the final Raster wil have. It will be use to determine its resolution.
#' @param extentExpansion Factor by which the final extent should be expended. If 0 and extent is NULL, the final extent will be determined by the most extreme locality records. If extent is given, extentExpansion will be ignored.
#' @param extent Object of class Extent delimiting the region to be included in the final raster
#' @param basemap Object of class Raster in which cell with NA values will not be included. If NULL, all cells in the raster will be included.
#' @return A Stack object with a layer for the number of records per cell and anotherone with the number of sampling events.
#' @examples
#' lopodRaster(LopodModel, metric="pp", metric="mean")
#' lopodRaster(LopodModel, metric="cellpres_i", quant=0.95)

xyToRaster =   function(xyRecords, xySamplingEffort, nrows = 50, extentExpansion = 0.1, extent = NULL, basemap = getData("worldclim", var="alt", res=10) ){

  if( class(xyRecords) != "SpatialPoints" & class(xyRecords) != "SpatialPointsDataFrame") stop("xyRecords should be object of class SpatialPoints or SpatialPointsDataFrame")
  if( class(xySamplingEffort) != "SpatialPoints" & class(xySamplingEffort) != "SpatialPointsDataFrame") stop("xySamplingEffort should be object of class SpatialPoints or SpatialPointsDataFrame")
  if( class(extent) != "Extent" & is.null(extent)==F) stop("extent should be object of class Extent or NULL")
  if( class(basemap) != "RasterLayer" & is.null(basemap)==F) stop("basemap should be object of class RasterLayer or NULL")
  if( class(extent) == "Extent") message("extentExpansion value will be ignored. Final extent might be expanded in the X axis")

  if (is.null(extent)){


    yRange =  xyRecords@bbox[2, "max"] - xyRecords@bbox[2, "min"]
    yExpandedRange = yRange * (1+extentExpansion)
    yExpansion = yRange * (extentExpansion/2)
    resolution =  yExpandedRange/nrows

    xRange =  xyRecords@bbox[1, "max"] - xyRecords@bbox[1, "min"]
    xExpandedRange = xRange * (1+extentExpansion)
    xExpansion = xRange * (extentExpansion/2)
    ncols = ceiling(xExpandedRange/resolution)

    ymin = xyRecords@bbox[2, "min"] - yExpansion
    xmin = xyRecords@bbox[1, "min"] - xExpansion

    ymax = ymin + nrows*resolution
    xmax = xmin + ncols*resolution

    baseRaster = raster(xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, resolution = resolution)
  }else{

    yRange =  extent@ymax - extent@ymin
    resolution =  yRange/nrows

    xRange =  extent@xmax - extent@xmin
    ncols = ceiling(xRange/resolution)

    ymin = extent@ymin
    xmin = extent@xmin

    ymax = ymin + nrows*resolution
    xmax = xmin + ncols*resolution

    baseRaster = raster(xmn = xmin, xmx = xmax, ymn = ymin, ymx = ymax, resolution = resolution)

  }

  detectCount = rasterize(xyRecords, baseRaster, fun=function(x,...)length(x) )
  detectCount [is.na(detectCount[])] = 0

  samplingCount = rasterize(xySamplingEffort, baseRaster, fun=function(x,...)length(x) )
  samplingCount [is.na(samplingCount[])] = 0

  if (min((samplingCount - detectCount)[], na.rm=T)<0 )stop ("Sampling effort must always be grater than number of detections")

  finalStack = stack(detectCount,samplingCount)
  names(finalStack) = c("spDetections","samplingEffort")

  if (is.null(basemap)==F){

    basemapRS = resample(x = basemap, y = baseRaster, method = "ngb" )
    finalStack[is.na(basemapRS[])] = NA
  }


  return(finalStack)


}



