#' An S4 class containing a LopodModel.
#' @import methods
#' @importClassesFrom rstan stanfit
#' @slot LopodData The LopodData object used to buid the LopodModel
#' @slot StanFit stanfit object with the run model
#' @slot modelInfo List of settings used to run the LopodModel


LopodModel = setClass("LopodModel",slots=c(LopodData = "LopodData", StanFit = "stanfit", modelInfo = "list" ) )
