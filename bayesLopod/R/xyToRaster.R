
xyToRaster =   function(xyRecords, xySamplingEffort, nrows = 50, extentExpansion = 0.1, extent = NULL, basemap = getData("worldclim", var="alt", res=10) ){

  if( class(xyRecords) != "SpatialPoints" & class(xyRecords) != "SpatialPointsDataFrame") stop("xyRecords should be object of class SpatialPoints or SpatialPointsDataFrame")
  if( class(xySamplingEffort) != "SpatialPoints" & class(xySamplingEffort) != "SpatialPointsDataFrame") stop("xySamplingEffort should be object of class SpatialPoints or SpatialPointsDataFrame")
  if( class(extent) != "Extent" & is.null(extent)==F) stop("extent should be object of class Extent or NULL")
  if( class(basemap) != "RasterLayer" & is.null(basemap)==F) stop("basemap should be object of class RasterLayer or NULL")
  if( class(extent) == "Extent") message("extentExpansion value will be ignored. Final extent might be expanded in the X axis")

  if (is.null(extent)){


    yRange =  xyRecords@bbox["Latitude", "max"] - xyRecords@bbox["Latitude", "min"]
    yExpandedRange = yRange * (1+extentExpansion)
    yExpansion = yRange * (extentExpansion/2)
    resolution =  yExpandedRange/nrows

    xRange =  xyRecords@bbox["Longitude", "max"] - xyRecords@bbox["Longitude", "min"]
    xExpandedRange = xRange * (1+extentExpansion)
    xExpansion = xRange * (extentExpansion/2)
    ncols = ceiling(xExpandedRange/resolution)

    ymin = xyRecords@bbox["Latitude", "min"] - yExpansion
    xmin = xyRecords@bbox["Longitude", "min"] - xExpansion

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



