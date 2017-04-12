\name{rasterLopodData}
\alias{rasterLopodData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rasterLopodData(rasterN, rasterY, Adjacency = T, extSample = 0.025, extDetection = 0.15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rasterN}{
%%     ~~Describe \code{rasterN} here~~
}
  \item{rasterY}{
%%     ~~Describe \code{rasterY} here~~
}
  \item{Adjacency}{
%%     ~~Describe \code{Adjacency} here~~
}
  \item{extSample}{
%%     ~~Describe \code{extSample} here~~
}
  \item{extDetection}{
%%     ~~Describe \code{extDetection} here~~
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
function (rasterN, rasterY, Adjacency = T, extSample = 0.025, 
    extDetection = 0.15) 
{
    if (extent(rasterN) != extent(rastery) | ncell(rasterN) != 
        ncell(rastery)) {
        stop("Raster for sampling effort and for detections should have the same extent and resolution")
    }
    if (min((rasterN - rastery)[], na.rm = T) < 0) {
        stop("Sampling effort must always be grater than number of detections")
    }
    if (extSample > 1 | extSample < 0 | extDetection > 1 | extDetection < 
        0) {
        stop("The extrapolation from sampled cells and those in which the speces has been detected must be between 0 and 1, the value is relative to the maximum distance in the raster")
    }
    maxDist = max(distanceFromPoints(rasterN, xyFromCell(rasterN, 
        1))[])
    if (Adjacency) {
        maxExtDistSample = maxDist * extSample
    }
    else {
        maxExtDistSample = 0
        if (extSample != 0) {
            message("No extrapolation to unsampled cells can be performed without adjacency matrix")
        }
    }
    maxExtDistDect = maxDist * extDetection
    DistSample = rasterN
    DistSample[] = NA
    DistSample[rasterN[] > 0] = 1
    DistSample = distance(DistSample)
    DistSample[DistSample[] > maxExtDistSample] = NA
    DistSample[DistSample[] <= maxExtDistSample] = 1
    DistDetec = rastery
    DistDetec[] = NA
    DistDetec[rastery[] > 0] = 1
    DistDetec = distance(DistDetec)
    DistDetec[DistDetec[] > maxExtDistDect] = NA
    DistDetec[DistDetec[] <= maxExtDistDect] = 1
    StudyArea = DistDetec * DistSample
    rasterN = rasterN * StudyArea
    rastery = rastery * StudyArea
    whichSampledCells = which(rasterN[] > 0)
    whichNotSampledCells = which(rasterN[] == 0)
    whichNoNACells = which(is.na(rasterN[]) == F)
    message(paste(sum(is.na(rasterN[])), "cells are NA - Dropped from analysis"))
    geoDataObject = stack(StudyArea, rasterN, rastery)
    names(geoDataObject) = c("studyArea", "samplingEffort", "Detections")
    if (Adjacency) {
        AdMAtrix = matrix(0, ncol = length(whichNoNACells), nrow = length(whichNoNACells))
        rownames(AdMAtrix) = as.character(whichNoNACells)
        colnames(AdMAtrix) = as.character(whichNoNACells)
        for (i in 1:length(whichNoNACells)) {
            adCells = as.character(adjacent(rasterN, cells = whichNoNACells[i], 
                pairs = F, directions = 8, target = whichNoNACells))
            AdMAtrix[as.character(whichNoNACells[i]), adCells] = 1
        }
        noNeighboursCells = which(colSums(AdMAtrix) == 0)
        noNeighboursCells = noNeighboursCells[names(noNeighboursCells)]
        if (length(noNeighboursCells) > 0) {
            AdMAtrix = AdMAtrix[-noNeighboursCells, ]
            AdMAtrix = AdMAtrix[, -noNeighboursCells]
        }
        message(paste(length(noNeighboursCells), "cells have no neighbours - Dropped from analysis"))
        nPairs = sum(AdMAtrix)/2
        sampledId = match(as.character(whichSampledCells), colnames(AdMAtrix))
        sampledId = data.frame(cellRaster = whichSampledCells, 
            cellStan = sampledId)
        whichIslandList = which(sampledId[, "cellRaster"] \%in\% 
            as.numeric(names(noNeighboursCells)))
        if (length(whichIslandList) > 0) {
            sampledId = sampledId[-whichIslandList, ]
        }
        notSampledId = match(as.character(whichNotSampledCells), 
            colnames(AdMAtrix))
        notSampledId = data.frame(cellRaster = whichNotSampledCells, 
            cellStan = notSampledId)
        whichIslandList = which(notSampledId[, "cellRaster"] \%in\% 
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
        sampledId = data.frame(cellRaster = whichSampledCells, 
            cellStan = 1:length(whichSampledCells))
        geoInfo = list(sampledId = sampledId)
    }
    return(LopodData_Class(geoDataObject = geoDataObject, geoInfo = geoInfo, 
        geoType = "Raster"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
