#' Trace
#'
#' @param LopodModel A LopodModel object
#' @param params Parameters to be plotted. Default is NULL, which plots all global parameters
#' @return A ggplot object.
#' @examples
#' lopodDens(LopodModel)


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

