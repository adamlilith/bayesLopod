#' Internal funtion to extract parameters from a StanModel object


modelParams = function(LopodModel){

  if(class(LopodModel) != "LopodModel") stop("Obeject needs to be a LopdModel")

  if (LopodModel@modelInfo$CAR == F) {

    if (LopodModel@modelInfo$varP == F){

      if (is.null(LopodModel@modelInfo$q)==T){

        globalPars = c("psy","p","q")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL

      }

      if (is.null(LopodModel@modelInfo$q)==F) {

        globalPars = c("psy","p")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL

      }
    }

    if (LopodModel@modelInfo$varP == T){

      if (is.null(LopodModel@modelInfo$q)==T){
        globalPars = c("psy","pmax","pmin","pRange","q")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL
      }

      if (is.null(LopodModel@modelInfo$q)==F) {
        globalPars = c("psy","pmax","pmin","pRange")
        sampledPars = c("psy_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y")
        allCellsPars = NULL
      }
    }
  }

  if (LopodModel@modelInfo$CAR == T) {

      if (LopodModel@modelInfo$varP == F){

        if (is.null(LopodModel@modelInfo$q)==T){
          globalPars = c("psy","p","q","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i")
          allCellsPars = c("psy_i")
        }

        if (is.null(LopodModel@modelInfo$q)==F) {
          globalPars = c("psy","p","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i")
          allCellsPars = c("psy_i")
        }
      }

      if (LopodModel@modelInfo$varP == T){

        if (is.null(LopodModel@modelInfo$q)==T){
          globalPars = c("psy","pmax","pmin","pRange","q","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i")
          allCellsPars = c("psy_i")
        }

        if (is.null(LopodModel@modelInfo$q)==F) {
          globalPars = c("psy","pmax","pmin","pRange","q","tau","alpha")
          sampledPars = c("pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i")
          allCellsPars = c("psy_i")
        }
      }
  }

  return(list(globalPars=globalPars,sampledPars=sampledPars,allCellsPars=allCellsPars))

  }



