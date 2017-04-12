
modelParams = function(LopodModel){
  
  if(class(LopodModel) != "LopodModel") stop("Obeject needs to be a LopdModel")
  
  if (LopodModel@modelInfo$CAR == F) {
    
    if (LopodModel@modelInfo$varP == F){
      
      if (is.null(LopodModel@modelInfo$q)==T){
        
        globalPars = c("psy","p","q","qRate")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL
        
      }
      
      if (is.null(LopodModel@modelInfo$q)==F) {
        
        globalPars = c("psy","p","qRate")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL
        
      }
    }
    
    if (LopodModel@modelInfo$varP == T){
      
      if (is.null(LopodModel@modelInfo$q)==T){
        globalPars = c("psy","pmax","pmin","pRange","q","qRate")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL
      }
      
      if (is.null(LopodModel@modelInfo$q)==F) {
        globalPars = c("psy","pmax","pmin","pRange","qRate")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL
      }
    }
  }
  
  if (LopodModel@modelInfo$CAR == T) {

      if (LopodModel@modelInfo$varP == F){
        
        if (is.null(LopodModel@modelInfo$q)==T){
          globalPars = c("psy","p","q","qRate","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y")
          allCellsPars = c("psy_i","pp","cellpres_i")
        }
        
        if (is.null(LopodModel@modelInfo$q)==F) {
          globalPars = c("psy","p","qRate","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y")
          allCellsPars = c("psy_i","pp","cellpres_i")
        }
      }
      
      if (LopodModel@modelInfo$varP == T){
        
        if (is.null(LopodModel@modelInfo$q)==T){
          globalPars = c("psy","pmax","pmin","pRange","q","qRate","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y")
          allCellsPars = c("psy_i","pp","cellpres_i")
        }
        
        if (is.null(LopodModel@modelInfo$q)==F) {
          globalPars = c("psy","pmax","pmin","pRange","q","qRate","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y")
          allCellsPars = c("psy_i","pp","cellpres_i")
        }
      }
  }
  
  return(list(globalPars=globalPars,sampledPars=sampledPars,allCellsPars=allCellsPars))
  
  }
  
  
  
