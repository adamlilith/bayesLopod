\name{lopodRaster}
\alias{lopodRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
lopodRaster(LopodModel, param, extrapolate = T, metric = NULL, quant = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LopodModel}{
%%     ~~Describe \code{LopodModel} here~~
}
  \item{param}{
%%     ~~Describe \code{param} here~~
}
  \item{extrapolate}{
%%     ~~Describe \code{extrapolate} here~~
}
  \item{metric}{
%%     ~~Describe \code{metric} here~~
}
  \item{quant}{
%%     ~~Describe \code{quant} here~~
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
function (LopodModel, param, extrapolate = T, metric = NULL, 
    quant = 0.5) 
{
    if (is.null(metric)) {
        columnName = paste(quant * 100, "\%", sep = "")
        probs = quant
    }
    if (is.null(metric) == F) {
        if ((metric \%in\% c("mean", "sd")) == F) 
            stop("metric can only be mean or sd")
        columnName = metric
        probs = NULL
        message(paste(metric, "will be returned. Value in quant (if any) will be ignored"))
    }
    if (class(LopodModel) != "LopodModel") 
        stop("Obeject needs to be a LopdModel")
    if (length(param) > 1) 
        stop("Only one parameter can be retuned as a raster at the time")
    if (LopodModel@LopodData@geoType != "Raster") 
        stop("Data is not in a Raster format")
    finalRaster = LopodModel@LopodData@geoDataObject[[1]]
    finalRaster[] = NA
    names(finalRaster) = columnName
    modelPar = modelParams(LopodModel)
    if (extrapolate == T) {
        if (LopodModel@modelInfo$CAR == F) 
            stop("Only CAR models can be extrapolated to unsampled units")
        if ((param \%in\% modelPar$allCellsPars) == F) 
            stop(paste("For this model only the following  parameters can be extrapolated into unsampled units:", 
                toString(modelPar$allCellsPars)))
        CellsID = rbind(LopodModel@LopodData@geoInfo$sampledId, 
            LopodModel@LopodData@geoInfo$notSampledId)
        ParObjects = paste(param, "[", CellsID$cellStan, "]", 
            sep = "")
        ParValues = summary(LopodModel@StanFit, pars = ParObjects, 
            probs = probs, use_cache = FALSE)$summary[, columnName]
        finalRaster[CellsID$cellRaster] = ParValues
    }
    if (extrapolate == F) {
        if ((param \%in\% c(modelPar$allCellsPars, modelPar$sampledPars)) == 
            F) 
            stop(paste("For this model only the following  parameters can be mapped:", 
                toString(c(modelPar$allCellsPars, modelPar$sampledPars))))
        CellsID = LopodModel@LopodData@geoInfo$sampledId
        ParObjects = paste(param, "[", CellsID$cellStan, "]", 
            sep = "")
        if ((param \%in\% modelPar$sampledPars) & (LopodModel@modelInfo$CAR == 
            T)) {
            ParObjects = paste(param, "[", 1:dim(CellsID)[1], 
                "]", sep = "")
        }
        ParValues = summary(LopodModel@StanFit, pars = ParObjects, 
            probs = probs, use_cache = FALSE)$summary[, columnName]
        finalRaster[CellsID$cellRaster] = ParValues
    }
    return(finalRaster)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
