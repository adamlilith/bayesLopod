#' Internal - Create a Raster object with Study Area from Raster data
#'
#' @param DistStak Stack object distance to Sampled cells and detections (in that order).
#' @param maxDist Maximun distance in Raster
#' @param extSample Number between 0 and 1. Maximum distance (relative to the diagonal of the raster) from a sampled cell that should be included in the study area. If 0, there is no extrapolation to unsampled cells.
#' @param extDetection Number between 0 and 1. Maximum distance (relative to the diagonal of the raster) from cell in which the species was detected that should be included in the study area. If 0, there is no extrapolation to unsampled cells.
#' @return A Raster object with Study Area.



rasterStudyArea = function(DistStak,maxDist, extSample = 0.025, extDetection = 0.15){




maxExtDistSample = maxDist * extSample
maxExtDistDect = maxDist * extDetection


DistSample = DistStak[["DistSample"]]
DistSample[] = NA
DistSample[DistStak[["DistSample"]][]==0] = 1
DistSample = distance(DistSample)
DistSample[DistSample[]>maxExtDistSample] = NA
DistSample[DistSample[]<=maxExtDistSample] = 1

DistDetec = DistStak[["DistDetec"]]
DistDetec[] = NA
DistDetec[DistStak[["DistDetec"]]==0] = 1
DistDetec = distance(DistDetec)
DistDetec[DistDetec[]>maxExtDistDect] = NA
DistDetec[DistDetec[]<=maxExtDistDect] = 1

StudyArea = DistDetec*DistSample


return(StudyArea)



}
