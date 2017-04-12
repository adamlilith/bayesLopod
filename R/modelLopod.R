
stanFilesDir = "./exec/"

modelLopod = function(LopodData, varP = F, q =  NULL, pmin = 0, CAR = F, nChains = 4, warmup = 2000, sampling = 1000, nCores=4){

#Check values anda data types
  
if (class(LopodData) != "LopodData") stop("Data should be entered in a LopodData Object")

if (is.null(q)==F){
  if (q<0 | q>1) stop("q must be NULL or between 0 and 1")
}
      
if (pmin<0 | pmin>1) stop("pmin must be between 0 and 1")

#Get N and y from different formats   
  
if(LopodData@geoType == "Raster"){
  
  N = LopodData@geoDataObject[["samplingEffort"]][LopodData@geoInfo$sampledId$cellRaster]
  y = LopodData@geoDataObject[["Detections"]][LopodData@geoInfo$sampledId$cellRaster]
  
}else{stop("Only Raster inputs are currently supported")}

  modelInfo = list(varP = varP, q =  q, CAR = CAR)
  
  #### Stan Files and Stan Data
  
  if (CAR == F) {
    
    if (varP == F){
      
      if (is.null(q)==T){
        
        stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                         sampledId = LopodData@geoInfo$sampledId$cellStan,
                         N = N,
                         y = y,
                         minP = pmin

        )
        
        message("Global p and q estimated. Psy for each sampling unit.")
        
        StanModel = stan_model(file = paste(stanFilesDir,"psyipq.stan",sep=""))
          
          
      }
      
      if (is.null(q)==F) {  
        
        if (pmin < q) pmin = q
        
        stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                         sampledId = LopodData@geoInfo$sampledId$cellStan,
                         N = N,
                         y = y,
                         minP = pmin,
                         q = q
                         
        )
        
        message("Global p estimated assuming p is larger than the given q (which can be 0, in which case there are no false detections). Psy estimated for each sampling unit.")
        StanModel = stan_model(file = paste(stanFilesDir,"psyip.stan",sep=""))
     
      }
    }
    
    if (varP == T){
      
      if (is.null(q)==T){
        
        
        stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                         sampledId = LopodData@geoInfo$sampledId$cellStan,
                         N = N,
                         y = y,
                         minP = pmin
                         
        )
        
        message("Global q estimated. Psy and P for each sampling unit")
      
          StanModel = stan_model(file = paste(stanFilesDir,"psyipiq.stan",sep=""))
          
         
      }
      
      if (is.null(q)==F) {  
        
        if (pmin < q) pmin = q
        
        stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                         sampledId = LopodData@geoInfo$sampledId$cellStan,
                         N = N,
                         y = y,
                         minP = pmin,
                         q = q
                         
        )
        
        message("Psy and p estimated for each sampling unit assuming p is larger than the given q (which can be 0, in which case there are no false detections).")
        
        StanModel = stan_model(file = paste(stanFilesDir,"psyipi.stan",sep=""))
        
       
      }
    }
  }
  
  if (CAR == T) {
    if(is.null(LopodData@geoInfo$W_sparse)==T){
      stop("Cannot perform CAR analysis without adjacency matrix")
    } else {
      
      if (varP == F){
        
        if (is.null(q)==T){
         
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                           notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                           n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                           W_n = dim(LopodData@geoInfo$W_sparse)[1],
                           W_sparse = LopodData@geoInfo$W_sparse,
                           D_sparse = LopodData@geoInfo$D_sparse,
                           lambda = LopodData@geoInfo$lambda_sparse,
                           N = N,
                           y = y,
                           minP = pmin
          )
          
          message("Global p and q estimated. Psy for each sampling unit. Psy is spatially autocorrelated.")
          
          StanModel = stan_model(file = paste(stanFilesDir,"psyipq_CAR.stan",sep=""))
            
        }
        
        if (is.null(q)==F) {  
          
          if (pmin < q) pmin = q
          
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                           notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                           n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                           W_n = dim(LopodData@geoInfo$W_sparse)[1],
                           W_sparse = LopodData@geoInfo$W_sparse,
                           D_sparse = LopodData@geoInfo$D_sparse,
                           lambda = LopodData@geoInfo$lambda_sparse,
                           N = N,
                           y = y,
                           minP = pmin, 
                           q = q
          )
          
          message("Global p estimated assuming p is larger than the given q (which can be 0, in which case there are no false detections). Psy estimated for each sampling unit. Psy is spatially autocorrelated.")
          
          StanModel = stan_model(file = paste(stanFilesDir,"psyip_CAR.stan",sep=""))
     
        }
      }
      
      if (varP == T){
        
        if (is.null(q)==T){
          
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                           notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                           n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                           W_n = dim(LopodData@geoInfo$W_sparse)[1],
                           W_sparse = LopodData@geoInfo$W_sparse,
                           D_sparse = LopodData@geoInfo$D_sparse,
                           lambda = LopodData@geoInfo$lambda_sparse,
                           N = N,
                           y = y,
                           minP = pmin
          )
          
          message("Global q estimated. Psy and P for each sampling unit. Psy is spatially autocorrelated.")
          
          StanModel = stan_model(file = paste(stanFilesDir,"psyipiq_CAR.stan",sep=""))

        }
        
        if (is.null(q)==F) {  
          
          if (pmin < q) pmin = q
          
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                           notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                           n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                           W_n = dim(LopodData@geoInfo$W_sparse)[1],
                           W_sparse = LopodData@geoInfo$W_sparse,
                           D_sparse = LopodData@geoInfo$D_sparse,
                           lambda = LopodData@geoInfo$lambda_sparse,
                           N = N,
                           y = y,
                           minP = pmin,
                           q = q
          )
          
          message("Psy and p estimated for each sampling unit assuming p is larger than the given q (which can be 0, in which case there are no false detections). Psy is spatially autocorrelated.")
          StanModel = stan_model(file = paste(stanFilesDir,"psyipi_CAR.stan",sep=""))
          
        }
      }
    }
  }

  StanFittetModel = sampling(StanModel,
           data = stanData,              # named list of data
           chains = nChains,                   # number of Markov chains
           warmup = warmup,               # number of warmup iterations per chain
           iter = sampling+warmup,                 # total number of iterations per chain
           cores = nCores,                    # number of cores
           refresh = 2,                 # show progress every 'refresh' iterations
           control = list(adapt_delta  = 0.8)
  )  
  return(LopodModel(LopodData = LopodData, StanFit = StanFittetModel, modelInfo = modelInfo))
}
