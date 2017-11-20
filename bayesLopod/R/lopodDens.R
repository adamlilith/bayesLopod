#' Kernel density estimates of global occupancy model parameters.
#'
#' @param LopodModel A LopodModel object
#' @param params Parameters to be plotted. Default is NULL, which plots all global parameters
#' @return A ggplot object.
#' @examples
#' data("mLopodRaster", package = "bayesLopod")
#' lopodDens(mLopodRaster, c("alpha", "tau"))
#'
#' data("mLopodShape", package = "bayesLopod")
#' lopodDens(mLopodShape, c("q", "pmin", "pmax"))

lopodDens=  function(LopodModel,params=NULL){

  #Summary for all global parameters is return if params is NULL

  if(class(LopodModel) != "LopodModel") stop("Obeject needs to be a LopdModel")

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

