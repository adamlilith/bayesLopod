#' Kernel density estimates of global occupancy model parameters.
#' @importFrom rstan stan_dens
#' @param LopodModel A \linkS4class{LopodModel} object
#' @param params Parameters to be plotted. Default is NULL, which plots all global parameters
#' @export
#' @return A ggplot object.
#' @examples
#'
#' \dontrun{
#' data("simSpRecords", package = "bayesLopod")
#' data("simSpSamplingEffort", package = "bayesLopod")
#' simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,
#' basemap = NULL, nrows = 50, extentExpansion = 0)
#' ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]],
#' rasterY = simSpRasters[["spDetections"]], Adjacency = TRUE )
#' mLopodRaster = modelLopod(LopodData = ld_Raster_adMatrix, varP = TRUE, q = NULL,
#' pmin = 0.1, CAR = FALSE, nChains = 4,warmup = 500,sampling = 100,nCores = 4)
#' lopodDens(mLopodRaster, c("alpha", "tau"))
#'
#' data("Andropogon_shape", package = "bayesLopod")
#' ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort",
#' fieldY = "detections",  Adjacency = TRUE, keepFields = FALSE)
#' mLopodShape = modelLopod(LopodData = ld_Shape, varP = TRUE, q = NULL,
#' pmin = 0, CAR = TRUE, nChains = 4,warmup = 500,sampling = 100,nCores =4)
#' lopodDens(mLopodShape, c("q", "pmin", "pmax"))
#' }

lopodDens=  function(LopodModel,params=NULL){

  #Summary for all global parameters is return if params is NULL

  if(class(LopodModel) != "LopodModel") stop("Object needs to be a LopdModel")

  modelPar = modelParams(LopodModel)
  sumPars = modelPar$globalPars

  if(is.null(params)==F){
    if(sum(params %in% sumPars)!=length(params)){
      stop(paste("For this model only the summary of the following global parameters can be returned:",toString(sumPars)))
    }else{
      sumPars = params
    }

  }

  stan_dens(LopodModel@StanFit, pars=sumPars)

}

