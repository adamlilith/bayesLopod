#' Create a LopodData object from Raster data
#' @importFrom slam  as.simple_triplet_matrix simple_triplet_diag_matrix crossprod_simple_triplet_matrix
#' @importFrom  rgeos gTouches
#' @param shape \linkS4class{SpatialPolygonsDataFrame} Object with at least two Fields corresponding to sampling effort and number of detections in each feature
#' @param effortField Field in shape corresponding to sampling effort (number of sampling events)in each feature.
#' @param detectField Field in shape corresponding to number of detections in each feature.
#' @param adjacency Boolean. If TRUE, and adjacency matrix is computed.
#' @param keepFields Boolean. If TRUE, other fields of the shape will be kept and 'effort' and 'detections' will be added. If FALSE, only the 'effort' and 'detections' will be kept in the LopodData Object.
#' @export
#' @return A \linkS4class{LopodData} object to be used in \code{\link{modelLopod}}.
#' @examples
#' data('Andropogon_shape', package = 'bayesLopod')
#' ld_Shape = shapeToLopodData(shape = Andropogon_shape, effortField = 'effort',
#' detectField = 'detections',  adjacency = FALSE, keepFields = FALSE)
#'
#' \dontrun{
#' data('Andropogon_shape', package = 'bayesLopod')
#' ld_Shape = shapeToLopodData(shape = Andropogon_shape, effortField = 'effort',
#' detectField = 'detections',  adjacency = TRUE, keepFields = TRUE)
#' }


shapeToLopodData = function(shape, effortField='effort', detectField='detections', adjacency = TRUE, keepFields = TRUE) {

  if (class(shape) != 'SpatialPolygonsDataFrame'){
    stop('Argument "shape" should be a SpatialPolygonsDataFrame object.')
  }


  if (min((shape@data[ , effortField] - shape@data[ , detectField])[], na.rm=TRUE) < 0){
    stop ('Sampling effort must always be grater than number of detections')

  }



# which cells have been sampled or not sampled?
whichSampledCells <- which(shape@data[ , effortField] > 0)
whichNotSampledCells <- which(shape@data[ , effortField] == 0)
whichNoNACells <- which(!is.na(shape@data[ , effortField]))

naFeats <- sum(is.na(rowSums(shape@data[ , c(detectField, effortField)])))
if (naFeats) message(paste(naFeatures, 'features have NA so will be dropped from the analysis.'))

# create geodata object
geoDataObject <- shape

if (keepFields) {

  geoDataObject@data[ , 'effort'] = shape@data[ , effortField]
  geoDataObject@data[ , 'detections'] = shape@data[ , detectField]


} else {

  geoDataObject@data = shape@data[ , c(effortField, detectField)]
  names(geoDataObject@data) = c('effort', 'detections')

}

geoDataObject@data[ , 'FeatureID'] <- 1:dim(geoDataObject@data)[1]

geoDataObject@data[ , 'FeatureID'] <- 1:dim(geoDataObject@data)[1]
row.names(geoDataObject) <- as.character(geoDataObject@data[ , 'FeatureID'])


if (adjacency){

  # adjacency matrix
  adjMatrixList <- rgeos::gTouches(geoDataObject,  byid = TRUE, returnDense=FALSE)
  featureShapeId = c(row.names(geoDataObject))
  adjMatrix = matrix(0, ncol = length(featureShapeId), nrow = length(featureShapeId))

  rownames(adjMatrix) <- as.character(featureShapeId)
  colnames(adjMatrix) <- as.character(featureShapeId)


  for (i in seq_along(featureShapeId)) {

    adCells <- adjMatrixList[[featureShapeId[i]]]
    adjMatrix[as.character(featureShapeId[i]), adCells] <- 1
    adjMatrix[adCells, as.character(featureShapeId[i])] <- 1

  }

  noNeighCells <- which(colSums(adjMatrix) == 0)
  noNeighCells <- noNeighCells[names(noNeighCells)]

  if (length(noNeighCells) > 0) {
    adjMatrix <- adjMatrix[-noNeighCells, ]
    adjMatrix <- adjMatrix[ , -noNeighCells]
  }

if (length(noNeighCells) > 0) message(paste(length(noNeighCells),'features have no neighbors so will be dropped from the analysis.'))

  nPairs <- sum(adjMatrix) / 2

  sampledId <- match(as.character(whichSampledCells), colnames(adjMatrix))
  sampledId <- data.frame(featureShape = whichSampledCells, cellStan = sampledId)
  whichIslandList <- which(sampledId[ , 'featureShape'] %in% as.numeric(names(noNeighCells)))

  if (length(whichIslandList) > 0){
    sampledId <- sampledId[-whichIslandList, ]
  }

  notSampledId <- match(as.character(whichNotSampledCells), colnames(adjMatrix))
  notSampledId <- data.frame(featureShape = whichNotSampledCells, cellStan = notSampledId)
  whichIslandList <- which(notSampledId[ , 'featureShape'] %in% as.numeric(names(noNeighCells)))

  if (length(whichIslandList) > 0) {
    notSampledId <- notSampledId[-whichIslandList, ]
  }


  allCellsId <- rbind(sampledId, notSampledId)
  allCellsId <- allCellsId[order(allCellsId[ , 'cellStan']), ]

  n <- length(sampledId[ , 'cellStan']) + length(notSampledId[ , 'cellStan'])


  # sparse representation of adjacency Matrix for Stan
  
  ### STOPPED HERE

  W_sparse =  matrix(0,nrow = nPairs, ncol = 2)

  counter = 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (adjMatrix[i, j] == 1) {
        W_sparse[counter, 1] = i;
        W_sparse[counter, 2] = j;
        counter = counter + 1;
      }
    }
  }

  D_sparse = rowSums(adjMatrix)


  w_sparse_mat= as.simple_triplet_matrix(adjMatrix)
  invsqrtD_SparseDiag = simple_triplet_diag_matrix(1 / sqrt(D_sparse))

  quadMatrix_sparse = crossprod_simple_triplet_matrix(crossprod_simple_triplet_matrix(w_sparse_mat,invsqrtD_SparseDiag),invsqrtD_SparseDiag)
  lambda_sparse = eigen(quadMatrix_sparse,only.values = TRUE)

  geoInfo = list(sampledId=sampledId,notSampledId=notSampledId,W_sparse=W_sparse,D_sparse=D_sparse,lambda_sparse=lambda_sparse$values)

} else {

  sampledId = data.frame('featureShape' = whichSampledCells, cellStan = 1:length(whichSampledCells))

  geoInfo = list(sampledId=sampledId)

}

return(LopodData_Class (geoDataObject = geoDataObject, geoInfo = geoInfo, geoType = 'shape' ))

}
