#' Train a Bayesian model of landscape occupancy from presence-only data
#'
#' @param data Object of class \code{lopodData} created with the function \code{\link[enmSdmBayesLopod]{makeLopodData}}.
#' @param varDetect Logical, if \code{TRUE}, then detectability will be allowed to vary between locations. If \code{FALSE} (default), detectability will be assumed to be the same across sites.
#' @param car Logical, if \code{TRUE} and if \code{data} contains an adjaceny matrix, then a conditional autoregression analysis component will be added to the occupancy component of the model.
#' @param detectMin Value between 0 and 1. Minimum value for detectability in a unit in which the species occurs. Default is 0.
#' @param warmup Positive integer, number of MCMC iterations to discard before sampling to estimate the posterior. Default is 2000.
#' @param sampling Positive integer, number of MCMC iterations to used to estimate the posterior (total number of iterations will be equal to \code{warmup + sampling}. Default is 1000.
#' @param thin Positive integer, number of iterations to skip when sampling from the 'sample' iterations. Useful for reducing autocorrelation in the samples.
#' @param chains Positive integer, number of Monte Carlo chains to run. Default is 4.
#' @param cores Positive integer, number of computer cores to use. Default is 1.
#' @export
#' @details
#' Parameters are estimated for the entire sampling area and can be examined using the \code{\link{lopodDensity}}, \code{\link{lopodSummary}} and \code{\link{lopodTrace}} functions in \pkg{enmSdmBayesLopod}.
#' return
#' \strong{Global values:}
#' \itemize{
#'   \item 'psi': Average occupancy across the study area.
#'   \item 'p': (if \code{varDetect = FALSE}) Probability of detection for all sampling units.
#'   \item 'detectMax': (if \code{varDetect = TRUE}) Maximum value of detectability.
#'   \item 'detectMin': (if \code{varDetect = TRUE}) Minimum value of p.
#'   \item 'alpha': (if \code{car = TRUE}) Alpha parameter of the conditional autoregressive model.
#'   \item 'tau': (if \code{car = TRUE}) Tau parameter of the conditional autoregressive model.
#'   \item 'chiSq'*: (EXPERIMENTAL) Chi-square statistic for model.
#'   \item 'll'*: (EXPERIMENTAL) Total log-likelihood for the pattern observed across all sampling units, given the parameters estimated in the model.
#'   \item 'aic'*: (EXPERIMENTAL) Akaike information criterion value for the model.
#' }
#'
#' \strong{Parameters specific to sample units with non-zero effort and with non-\code{NA} detections:}
#' These parameters are estimated for each sample unit with at least one sampling event. These parameters can be returned to a data frame, SpatialPolygonsDataFrame, SpatialLinesDataFrame, or raster using the \code{\link{lopodReform}} function.
#' \itemize{
#'   \item 'psiSampled': Probability of occurrence in each sampling unit.
#'   \item 'detectCorrect': (if \code{varDetect = TRUE}) probability of detection in each sampling unit that is occupied. This value is corrected to include unoccupied cells, in which case p = 0.
#'   \item 'detectSim': Simulated number of detections based on the number of sampling events and the model.
#'   \item 'detectTrueSim': Simulated number of true detections based on the number of sampling events and the model.
#'   \item 'detectFalseSim': Simulated number of false detections based on the number of sampling events and the model.
#'   \item 'pp': Probability of presence given a pattern of sampling effort/detections and the model.
#'   \item 'cellPresSim': Simulated presence/absence for each iteration based on model.
#'   \item 'detectExpect'*: (EXPERIMENTAL) Expected number of detections.
#'   \item 'llSite'*: (EXPERIMENTAL) Likelihood of the pattern in a site.
#' }
#'
#' \strong{Parameters for all sample units:}
#' These parameters are estimated for all sampling units if a conditional autoregressive model is implemented (argument \code{car = TRUE}). These parameters can be returned to a data frame, SpatialPolygonsDataFrame, SpatialLinesDataFrame, or raster using the \code{\link{lopodReform}} function.
#' \itemize{
#'   \item 'psiCar': Probability of occurrence in each sampling units. This is inferred for unsampled united using the CAR comnponent. For sampled units, 'psiCar' equals 'psiSampled'.
#' }
#' *EXPERIMENTAL VALUES SHOULD BE INTERPRETED CAREFULLY TO DETERMINE WHICH MODELS ARE BETTER THAN OTHERS.
#' @examples

load('C:/Ecology/Drive/R/enmSdmBayesLopod/data/andropogon.rda')
data <- makeLopodData(andropogon, 'detections', 'effort')


#' 

modelLopod <- function(
	data,
	varDetect = FALSE,
	detectMin = 0,
	car = FALSE,
	warmup = 2000,
	sampling = 1000,
	chains = 4,
	cores = 1
) {

	out <- data

	# number of cores
	cores <- min(cores, chains, parallel::detectCores())

	# compile data object
	if (car & class(data$adj) != 'matrix') {
		car <- FALSE
		message('Argument "car" is "TRUE" but the "data" object does not contain an adjacency matrix, so CAR will be not be implemented.')
	} else if (car) {
		adj <- data$adj
	} else {
		adj <- NA
	}
	
	data <- list(
		sites = dim(data$data)[1L],
		detects = data$data$detects,
		effort = data$data$efforts
	)
	
	if (car) data <- c(data, list(adj=adj, adjDim = dim(adj)[1]))
	
}
