
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
  
  return(summary(LopodModel@StanFit, pars=sumPars, probs=probs, use_cache=FALSE)$summary)
  
}

