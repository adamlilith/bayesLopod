#' Internal function to extract parameters from a StanModel object
#' @param LopodModel A LopodModel object
#' @return A  list of parameters.




modelParams = function(LopodModel){

  if(class(LopodModel) != "LopodModel") stop("Object needs to be a LopdModel")

  if (LopodModel@modelInfo$CAR == F) {

    if (LopodModel@modelInfo$varP == F){

      if (is.null(LopodModel@modelInfo$q)==T){

        globalPars = c("psi","p","q","chi_sq","lLh","AIC")
        sampledPars = c("psi_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y","expRec","lLh_cell")
        allCellsPars = NULL

      }

      if (is.null(LopodModel@modelInfo$q)==F) {

        globalPars = c("psi","p","chi_sq","lLh","AIC")
        sampledPars = c("psi_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y","expRec","lLh_cell")
        allCellsPars = NULL

      }
    }

    if (LopodModel@modelInfo$varP == T){

      if (is.null(LopodModel@modelInfo$q)==T){
        globalPars = c("psi","pmax","pmin","q","chi_sq","lLh","AIC")
        sampledPars = c("psi_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y","expRec","lLh_cell")
        allCellsPars = NULL
      }

      if (is.null(LopodModel@modelInfo$q)==F) {
        globalPars = c("psi","pmax","pmin","chi_sq","lLh","AIC")
        sampledPars = c("psi_Sampled","pCorr","pp","cellpres_i","sim_y","sim_true_y","sim_false_y","expRec","lLh_cell")
        allCellsPars = NULL
      }
    }
  }

  if (LopodModel@modelInfo$CAR == T) {

      if (LopodModel@modelInfo$varP == F){

        if (is.null(LopodModel@modelInfo$q)==T){
          globalPars = c("psi","p","q","tau","alpha","chi_sq","lLh","AIC")
          sampledPars = c("psi_Sampled", "pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i","expRec","lLh_cell")
          allCellsPars = c("psi_i")
        }

        if (is.null(LopodModel@modelInfo$q)==F) {
          globalPars = c("psi","p","tau","alpha","chi_sq","lLh","AIC")
          sampledPars = c("psi_Sampled", "pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i","expRec","lLh_cell")
          allCellsPars = c("psi_i")
        }
      }

      if (LopodModel@modelInfo$varP == T){

        if (is.null(LopodModel@modelInfo$q)==T){
          globalPars = c("psi","pmax","pmin","q","tau","alpha","chi_sq","lLh","AIC")
          sampledPars = c("psi_Sampled","pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i","expRec","lLh_cell")
          allCellsPars = c("psi_i")
        }

        if (is.null(LopodModel@modelInfo$q)==F) {
          globalPars = c("psi","pmax","pmin","tau","alpha","chi_sq","lLh","AIC")
          sampledPars = c("psi_Sampled","pCorr","sim_y","sim_true_y","sim_false_y","pp","cellpres_i","expRec","lLh_cell")
          allCellsPars = c("psi_i")
        }
      }
  }

  return(list(globalPars=globalPars,sampledPars=sampledPars,allCellsPars=allCellsPars))

  }
