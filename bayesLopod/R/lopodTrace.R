#' Plots the values of model parameters for each chain across iterations
#'
#' @param LopodModel A LopodModel object
#' @param params Parameters to be plotted. Default is "lp__" which plots the log posterior probability
#' @param inc_warmup Boolean. If true, warm-up iterations are plotted. Default is FALSE.
#' @return A ggplot object.
#' @examples
#' data("mLopodRaster", package = "bayesLopod"
#' lopodTrace(mLopodRaster, inc_warmup = F, params = c("p","q"))
#'
#' data("mLopodShape", package = "bayesLopod")
#' lopodTrace(mLopodShape, inc_warmup = T)



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

