
lopodRaster =  function(LopodModel,param,extrapolate=T, metric = NULL, quant=0.5){

  if (is.null(metric)){
    columnName=paste(quant*100,"%", sep="")
    probs = quant
    
  }
  if (is.null(metric)==F){
    if((metric %in% c("mean", "sd"))==F)stop("metric can only be mean or sd")
    
    columnName=metric
    probs = NULL
    
    message(paste(metric,"will be returned. Value in quant (if any) will be ignored"))
    
  }
  
  if(class(LopodModel) != "LopodModel") stop("Obeject needs to be a LopdModel")
  if(length(param)>1) stop("Only one parameter can be retuned as a raster at the time")
  if(LopodModel@LopodData@geoType != "Raster") stop("Data is not in a Raster format")
  
  finalRaster = LopodModel@LopodData@geoDataObject[[1]]
  finalRaster[] = NA
  names(finalRaster) = columnName
  
 
  modelPar = modelParams(LopodModel)
  
  if(extrapolate==T){
    if(LopodModel@modelInfo$CAR==F) stop("Only CAR models can be extrapolated to unsampled units")
    
    if((param %in% modelPar$allCellsPars)==F) stop(paste("For this model only the following  parameters can be extrapolated into unsampled units:",toString(modelPar$allCellsPars)))
    
    
    CellsID = rbind(LopodModel@LopodData@geoInfo$sampledId,LopodModel@LopodData@geoInfo$notSampledId)
    ParObjects=paste(param,"[",CellsID$cellStan,"]",sep="")
    
    ParValues = summary(LopodModel@StanFit,pars=ParObjects,probs=probs, use_cache=FALSE)$summary[,columnName]
    finalRaster[CellsID$cellRaster] = ParValues
      

      
    }
    
  if(extrapolate==F){
  if((param %in% c(modelPar$allCellsPars,modelPar$sampledPars))==F) stop(paste("For this model only the following  parameters can be mapped:",toString(c(modelPar$allCellsPars,modelPar$sampledPars))))
  
    CellsID = LopodModel@LopodData@geoInfo$sampledId
    ParObjects=paste(param,"[",CellsID$cellStan,"]",sep="")
    
    
  if ((param %in% modelPar$sampledPars)&(LopodModel@modelInfo$CAR==T)){
  
  ParObjects=paste(param,"[",1:dim(CellsID)[1],"]",sep="")
    
  }
    
  
  ParValues = summary(LopodModel@StanFit,pars=ParObjects,probs=probs, use_cache=FALSE)$summary[,columnName]
  finalRaster[CellsID$cellRaster] = ParValues
  
  
  }

  return(finalRaster)
  
}

