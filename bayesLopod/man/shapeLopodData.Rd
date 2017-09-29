\name{shapeLopodData}
\alias{shapeLopodData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shapeLopodData(Shapefile, fieldN = "sampEffort", fieldY = "detections", Adjacency = T, keepFields = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Shapefile}{
%%     ~~Describe \code{Shapefile} here~~
}
  \item{fieldN}{
%%     ~~Describe \code{fieldN} here~~
}
  \item{fieldY}{
%%     ~~Describe \code{fieldY} here~~
}
  \item{Adjacency}{
%%     ~~Describe \code{Adjacency} here~~
}
  \item{keepFields}{
%%     ~~Describe \code{keepFields} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Shapefile, fieldN = "sampEffort", fieldY = "detections", 
    Adjacency = T, keepFields = T) 
{
    if (class(Shapefile) != "SpatialPolygonsDataFrame") {
        stop("Shapefile should be a SpatialPolygonsDataFrame Object")
    }
    if (min((Shapefile@data[, fieldN] - Shapefile@data[, fieldY])[], 
        na.rm = T) < 0) {
        stop("Sampling effort must always be grater than number of detections")
    }
    whichSampledCells = which(Shapefile@data[, fieldN] > 0)
    whichNotSampledCells = which(Shapefile@data[, fieldN] == 
        0)
    whichNoNACells = which(is.na(Shapefile@data[, fieldN]) == 
        F)
    message(paste(sum(is.na(Shapefile@data[, fieldN])), "cells are NA - Dropped from analysis"))
    geoDataObject = Shapefile
    if (keepFields) {
        geoDataObject@data[, "sampEffort"] = Shapefile@data[, 
            fieldN]
        geoDataObject@data[, "detections"] = Shapefile@data[, 
            fieldY]
    }
    else {
        geoDataObject@data = Shapefile@data[, c(fieldN, fieldY)]
        names(geoDataObject@data) = c("sampEffort", "detections")
    }
    geoDataObject@data[, "FeatureID"] = 1:dim(geoDataObject@data)[1]
    geoDataObject@data[, "FeatureID"] = 1:dim(geoDataObject@data)[1]
    row.names(geoDataObject) = as.character(geoDataObject@data[, 
        "FeatureID"])
    if (Adjacency) {
        AdMAtrixList = gTouches(geoDataObject, byid = T, returnDense = F)
        FeaturesShapeId = c(row.names(geoDataObject))
        AdMAtrix = matrix(0, ncol = length(FeaturesShapeId), 
            nrow = length(FeaturesShapeId))
        rownames(AdMAtrix) = as.character(FeaturesShapeId)
        colnames(AdMAtrix) = as.character(FeaturesShapeId)
        for (i in 1:length(FeaturesShapeId)) {
            adCells = AdMAtrixList[[FeaturesShapeId[i]]]
            AdMAtrix[as.character(FeaturesShapeId[i]), adCells] = 1
            AdMAtrix[adCells, as.character(FeaturesShapeId[i])] = 1
        }
        noNeighboursCells = which(colSums(AdMAtrix) == 0)
        noNeighboursCells = noNeighboursCells[names(noNeighboursCells)]
        if (length(noNeighboursCells) > 0) {
            AdMAtrix = AdMAtrix[-noNeighboursCells, ]
            AdMAtrix = AdMAtrix[, -noNeighboursCells]
        }
        message(paste(length(noNeighboursCells), "features have no neighbours - Dropped from analysis"))
        nPairs = sum(AdMAtrix)/2
        sampledId = match(as.character(whichSampledCells), colnames(AdMAtrix))
        sampledId = data.frame(featureShape = whichSampledCells, 
            cellStan = sampledId)
        whichIslandList = which(sampledId[, "featureShape"] \%in\% 
            as.numeric(names(noNeighboursCells)))
        if (length(whichIslandList) > 0) {
            sampledId = sampledId[-whichIslandList, ]
        }
        notSampledId = match(as.character(whichNotSampledCells), 
            colnames(AdMAtrix))
        notSampledId = data.frame(featureShape = whichNotSampledCells, 
            cellStan = notSampledId)
        whichIslandList = which(notSampledId[, "featureShape"] \%in\% 
            as.numeric(names(noNeighboursCells)))
        if (length(whichIslandList) > 0) {
            notSampledId = notSampledId[-whichIslandList, ]
        }
        AllCellsId = rbind(sampledId, notSampledId)
        AllCellsId = AllCellsId[order(AllCellsId[, "cellStan"]), 
            ]
        n = length(sampledId[, "cellStan"]) + length(notSampledId[, 
            "cellStan"])
        W_sparse = matrix(0, nrow = nPairs, ncol = 2)
        counter = 1
        for (i in 1:(n - 1)) {
            for (j in (i + 1):n) {
                if (AdMAtrix[i, j] == 1) {
                  W_sparse[counter, 1] = i
                  W_sparse[counter, 2] = j
                  counter = counter + 1
                }
            }
        }
        D_sparse = rowSums(AdMAtrix)
        w_sparse_mat = as.simple_triplet_matrix(AdMAtrix)
        invsqrtD_SparseDiag = simple_triplet_diag_matrix(1/sqrt(D_sparse))
        quadMatrix_sparse = crossprod_simple_triplet_matrix(crossprod_simple_triplet_matrix(w_sparse_mat, 
            invsqrtD_SparseDiag), invsqrtD_SparseDiag)
        lambda_sparse = eigen(quadMatrix_sparse, only.values = T)
        geoInfo = list(sampledId = sampledId, notSampledId = notSampledId, 
            W_sparse = W_sparse, D_sparse = D_sparse, lambda_sparse = lambda_sparse$values)
    }
    else {
        sampledId = data.frame(featureShape = whichSampledCells, 
            cellStan = 1:length(whichSampledCells))
        geoInfo = list(sampledId = sampledId)
    }
    return(LopodData_Class(geoDataObject = geoDataObject, geoInfo = geoInfo, 
        geoType = "Shapefile"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
