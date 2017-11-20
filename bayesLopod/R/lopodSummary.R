#' Summary statistics for a LopodModel
#'
#' @param LopodModel A LopodModel object
#' @param params Parameters to be plotted. Default is NULL, which plots all global parameters
#' @param probs Quantiles to be estimated
#' @return  Returns a dataframe which contain summaries for for all chains merged for the Global Parameters of a LopodModel. Included in the summary are quantiles, means, standard deviations (sd), effective sample sizes (n_eff), Monte Carlo standard errors (se_mean) and Rhats.
#' @export
#' @examples
#' data("mLopodRaster", package = "bayesLopod")
#' lopodSummary(mLopodRaster, params = c("psy","p","q"))
#'
#' data("mLopodShape", package = "bayesLopod")
#' lopodSummary(mLopodRaster)



lopodSummary =  function(LopodModel,params=NULL,  probs = c(0.05, 0.50, 0.95)){

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


  return(rstan::summary(LopodModel@StanFit, pars=sumPars, probs=probs, use_cache=FALSE)$summary)

}

