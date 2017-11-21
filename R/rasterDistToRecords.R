#' Internal - Create a Stack with distance matrices to sampled and detected cells
#'
#' @importFrom raster distance
#' @param rasterN Raster object with sampling effort (number of sampling events)in each cell.
#' @param rasterY Raster object with number of detections in each cell.
#' @return A Stack object with distance to sampled cells and distance to detections.



rasterDistToRecords = function(rasterN, rasterY){


DistSample = rasterN
DistSample[] = NA
DistSample[rasterN[]>0] = 1
DistSample = distance(DistSample)

DistDetec = rasterY
DistDetec[] = NA
DistDetec[rasterY[]>0] = 1
DistDetec = distance(DistDetec)

outStack = stack(DistSample,DistDetec)
names(outStack) = c("DistSample", "DistDetec")

return(outStack)



}
