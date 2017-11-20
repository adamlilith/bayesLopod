#' Plots the values of model parameters for each chain across iterations
#'
#' @param LopodModel A LopodModel object
#' @param params Parameters to be plotted. Default is "lp__" which plots the log posterior probability
#' @param inc_warmup Boolean. If true, warm-up iterations are plotted. Default is FALSE.
#' @return A ggplot object.
#' @examples
#' lopodSummary(LopodModel, probs = NULL)
#'
#' \dontrun{
#' geocode("3817 Spruce St, Philadelphia, PA 19104")
#' geocode("Philadelphia, PA")
#' dat <- data.frame(value=runif(3),address=c("3817 Spruce St, Philadelphia, PA 19104","Philadelphia, PA","Neverneverland"))
#' geocode(dat)
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

