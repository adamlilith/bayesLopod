#' Create a LopodDAta object from Raster data
#'
#' @param Shapefile SpatialPolygonsDataFrame Object with at least two Fields corresponding to sampling effort and number of detections in each feature
#' @param fieldN Field in Shapefile corresponding to sampling effort (number of sampling events)in each feature.
#' @param fieldY Field in Shapefile corresponding to number of detections in each feature.
#' @param Adjacency Boolean. If TRUE, and adjancency matrix is computed.
#' @param Adjacency Boolean. If TRUE, other fields of the Shapefile will be kept and "sampEffort" and "detections" will be added. If FALSE, only the "sampEffort" and "detections" will be kept in the LopodData Object.
#' @return A LopodData object to be used in modelLopod.
#' @examples
#' lopodSummary(LopodModel, probs = NULL)
#'
#' \dontrun{
#' geocode("3817 Spruce St, Philadelphia, PA 19104")
#' geocode("Philadelphia, PA")
#' dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
#' geocode(dat)
#' }


shapeLopodData = function(Shapefile,fieldN="sampEffort", fieldY="detections",Adjacency = T, keepFields = T){

  if (class(Shapefile) != "SpatialPolygonsDataFrame"){
    stop ("Shapefile should be a SpatialPolygonsDataFrame Object")

  }


  if (min((Shapefile@data[,fieldN] - Shapefile@data[,fieldY])[], na.rm=T)<0 ){
    stop ("Sampling effort must always be grater than number of detections")

  }



#Which cells have been sampled or not sampled
whichSampledCells = which(Shapefile@data[,fieldN]>0)

whichNotSampledCells = which(Shapefile@data[,fieldN]==0)

whichNoNACells = which(is.na(Shapefile@data[,fieldN]) == F)
message(paste(sum(is.na(Shapefile@data[,fieldN])),"cells are NA - Dropped from analysis"))

geoDataObject = Shapefile

if (keepFields) {

  geoDataObject@data[,"sampEffort"] = Shapefile@data[,fieldN]
  geoDataObject@data[,"detections"] = Shapefile@data[,fieldY]


} else {

  geoDataObject@data = Shapefile@data[,c(fieldN,fieldY)]
  names(geoDataObject@data) = c("sampEffort","detections")

}

geoDataObject@data[,"FeatureID"] = 1:dim(geoDataObject@data)[1]

geoDataObject@data[,"FeatureID"] = 1:dim(geoDataObject@data)[1]
row.names(geoDataObject) = as.character(geoDataObject@data[,"FeatureID"])


if (Adjacency){


  #Adjacency Matrix
  AdMAtrixList = gTouches(geoDataObject,  byid = T, returnDense=F)
  FeaturesShapeId = c(row.names(geoDataObject))
  AdMAtrix = matrix(0,ncol=length(FeaturesShapeId),nrow = length(FeaturesShapeId) )

  rownames(AdMAtrix)=as.character(FeaturesShapeId)
  colnames(AdMAtrix)=as.character(FeaturesShapeId)


  for ( i in 1:length(FeaturesShapeId)){

    adCells =  AdMAtrixList[[FeaturesShapeId[i]]]
    AdMAtrix[as.character(FeaturesShapeId[i]),adCells] = 1
    AdMAtrix[adCells,as.character(FeaturesShapeId[i])] = 1

  }

  noNeighboursCells = which(colSums(AdMAtrix)==0)
  noNeighboursCells = noNeighboursCells[names(noNeighboursCells)]

  if (length(noNeighboursCells) >0 ){
    AdMAtrix = AdMAtrix[-noNeighboursCells,]
    AdMAtrix = AdMAtrix[,-noNeighboursCells]
  }

  message(paste(length(noNeighboursCells),"features have no neighbours - Dropped from analysis"))


  nPairs = sum(AdMAtrix)/2

  sampledId = match(as.character(whichSampledCells),colnames(AdMAtrix))
  sampledId = data.frame(featureShape = whichSampledCells, cellStan = sampledId )
  whichIslandList=which(sampledId[,"featureShape"]%in%as.numeric(names(noNeighboursCells)))

  if (length(whichIslandList) >0 ){
    sampledId = sampledId[-whichIslandList,]
  }


  notSampledId = match(as.character(whichNotSampledCells),colnames(AdMAtrix))
  notSampledId = data.frame(featureShape = whichNotSampledCells, cellStan = notSampledId)
  whichIslandList=which(notSampledId[,"featureShape"]%in%as.numeric(names(noNeighboursCells)))

  if (length(whichIslandList) >0 ){
    notSampledId = notSampledId[-whichIslandList,]
  }


  AllCellsId = rbind(sampledId,notSampledId)
  AllCellsId = AllCellsId[order(AllCellsId[,"cellStan"]),]

  n = length(sampledId[,"cellStan"])+length(notSampledId[,"cellStan"])


  ##Sparse representation of ad Matrix for Stan


  W_sparse =  matrix(0,nrow = nPairs, ncol = 2)

  counter = 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (AdMAtrix[i, j] == 1) {
        W_sparse[counter, 1] = i;
        W_sparse[counter, 2] = j;
        counter = counter + 1;
      }
    }
  }

  D_sparse = rowSums(AdMAtrix)


  w_sparse_mat= as.simple_triplet_matrix(AdMAtrix)
  invsqrtD_SparseDiag = simple_triplet_diag_matrix(1 / sqrt(D_sparse))

  quadMatrix_sparse = crossprod_simple_triplet_matrix(crossprod_simple_triplet_matrix(w_sparse_mat,invsqrtD_SparseDiag),invsqrtD_SparseDiag)
  lambda_sparse = eigen(quadMatrix_sparse,only.values = T)

  geoInfo = list(sampledId=sampledId,notSampledId=notSampledId,W_sparse=W_sparse,D_sparse=D_sparse,lambda_sparse=lambda_sparse$values)

} else {

  sampledId = data.frame("featureShape" = whichSampledCells, cellStan = 1:length(whichSampledCells))

  geoInfo = list(sampledId=sampledId)

}

return(LopodData_Class (geoDataObject = geoDataObject, geoInfo = geoInfo, geoType = "Shapefile" ))



}
