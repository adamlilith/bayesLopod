#' Plots the values of model parameters for each chain across iterations
#' @importFrom rstan stan_trace
#' @param LopodModel A \linkS4class{LopodModel} object
#' @param params Parameters to be plotted. Default is "lp__" which plots the log posterior probability
#' @param inc_warmup Boolean. If true, warm-up iterations are plotted. Default is FALSE.
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
#' lopodTrace(mLopodRaster, inc_warmup = FALSE, params = c("p","q"))
#'
#' data("Andropogon_shape", package = "bayesLopod")
#' ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort",
#' fieldY = "detections",  Adjacency = TRUE, keepFields = FALSE)
#' mLopodShape = modelLopod(LopodData = ld_Shape, varP = TRUE, q = NULL,
#' pmin = 0, CAR = TRUE, nChains = 4,warmup = 500,sampling = 100,nCores =4)
#' lopodTrace(mLopodShape, inc_warmup = TRUE)
#' }




lopodTrace=  function(LopodModel,params="lp__", inc_warmup = FALSE){


  if(class(LopodModel) != "LopodModel") stop("Object needs to be a LopodModel")

  modelPar = modelParams(LopodModel)
  sumPars = c(modelPar$globalPars, "lp__")

  if(is.null(params)==F){
    if(sum(params %in% sumPars)!=length(params)){
      stop(paste("For this model only the summary of the following global parameters can be returned:",toString(sumPars)))
    }else{
      sumPars = params
    }

  }

  stan_trace(LopodModel@StanFit, pars=sumPars, inc_warmup = inc_warmup)

}

