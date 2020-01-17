#' Train a Bayesian model of landscape occupancy from presence-only data
#'
#' @param data Object of class \code{lopodData} created with the function \code{\link[enmSdmBayesLopod]{makeLopodData}}.
#' @param varDetect Logical, if \code{TRUE}, then detectability will be allowed to vary between locations. If \code{FALSE} (default), detectability will be assumed to be the same across sites.
#' @param car Logical, if \code{TRUE} and if \code{data} contains an adjaceny matrix, then a conditional autoregression analysis component will be added to the occupancy component of the model.
#' @param detectMin Value between 0 and 1. Minimum value for detectability in a unit in which the species occurs. Default is 0.
#' @param warmupIters Positive integer, number of MCMC iterations to discard before sampleIters to estimate the posterior. Default is 2000.
#' @param sampleIters Positive integer, number of MCMC iterations to used to estimate the posterior (total number of iterations will be equal to \code{warmupIters + sampleIters}. Default is 1000.
#' @param thin Positive integer, number of iterations to skip when sampleIters from the 'sample' iterations. Useful for reducing autocorrelation in the samples.
#' @param chains Positive integer, number of Monte Carlo chains to run. Default is 4.
#' @param cores Positive integer, number of computer cores to use. Default is 1.
#' @param verbose Logical, if \code{TRUE} (default), then display progress.
#' @export
#' @details
#' Parameters are estimated for the entire sampleIters area and can be examined using the \code{\link{lopodDensity}}, \code{\link{lopodSummary}} and \code{\link{lopodTrace}} functions in \pkg{enmSdmBayesLopod}.
#' return
#' \strong{Global values:}
#' \itemize{
#'   \item 'psi': Average occupancy across the study area.
#'   \item 'p': (if \code{varDetect = FALSE}) Probability of detection for all sampleIters units.
#'   \item 'detectMax': (if \code{varDetect = TRUE}) Maximum value of detectability.
#'   \item 'detectMin': (if \code{varDetect = TRUE}) Minimum value of p.
#'   \item 'alpha': (if \code{car = TRUE}) Alpha parameter of the conditional autoregressive model.
#'   \item 'tau': (if \code{car = TRUE}) Tau parameter of the conditional autoregressive model.
#'   \item 'chiSq'*: (EXPERIMENTAL) Chi-square statistic for model.
#'   \item 'll'*: (EXPERIMENTAL) Total log-likelihood for the pattern observed across all sampleIters units, given the parameters estimated in the model.
#'   \item 'aic'*: (EXPERIMENTAL) Akaike information criterion value for the model.
#' }
#'
#' \strong{Parameters specific to sample units with non-zero effort and with non-\code{NA} detections:}
#' These parameters are estimated for each sample unit with at least one sampleIters event. These parameters can be returned to a data frame, SpatialPolygonsDataFrame, SpatialLinesDataFrame, or raster using the \code{\link{lopodReform}} function.
#' \itemize{
#'   \item 'psiSampled': Probability of occurrence in each sampleIters unit.
#'   \item 'detectCorrect': (if \code{varDetect = TRUE}) probability of detection in each sampleIters unit that is occupied. This value is corrected to include unoccupied cells, in which case p = 0.
#'   \item 'detectSim': Simulated number of detections based on the number of sampleIters events and the model.
#'   \item 'detectTrueSim': Simulated number of true detections based on the number of sampleIters events and the model.
#'   \item 'detectFalseSim': Simulated number of false detections based on the number of sampleIters events and the model.
#'   \item 'pp': Probability of presence given a pattern of sampleIters effort/detections and the model.
#'   \item 'cellPresSim': Simulated presence/absence for each iteration based on model.
#'   \item 'detectExpect'*: (EXPERIMENTAL) Expected number of detections.
#'   \item 'llSite'*: (EXPERIMENTAL) Likelihood of the pattern in a site.
#' }
#'
#' \strong{Parameters for all sample units:}
#' These parameters are estimated for all sampleIters units if a conditional autoregressive model is implemented (argument \code{car = TRUE}). These parameters can be returned to a data frame, SpatialPolygonsDataFrame, SpatialLinesDataFrame, or raster using the \code{\link{lopodReform}} function.
#' \itemize{
#'   \item 'psiCar': Probability of occurrence in each sampleIters units. This is inferred for unsampled united using the CAR comnponent. For sampled units, 'psiCar' equals 'psiSampled'.
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
	warmupIters = 2000,
	sampleIters = 1000,
	thin = 1,
	chains = 4,
	cores = 1,
	verbose = TRUE
) {

	origData <- data

	### number of cores
	cores <- min(cores, chains, parallel::detectCores())

	# ### construct constants object
	# consts <- list(
		# numSites = nrow(data$data)
	# )
	
	### compile data object
	if (car & class(data$adj) != 'matrix') {
		car <- FALSE
		message('Argument "car" is "TRUE" but the "data" object does not contain an adjacency matrix, so CAR will be not be implemented.')
	} else if (car) {
		adj <- data$adj
	} else {
		adj <- NA
	}

	data <- list(
		numSites = dim(data$data)[1L],
		detects = data$data$detects,
		effort = data$data$efforts
	)
	
	if (car) data <- c(data, list(adj=adj, adjDim = dim(adj)[1]))

	# ### construct initializations object
	# inits <- list(
		# psi = rep(0.5, n),
		# trueOcc = round(runif(n)),
		# p = rep(0.2, n)
	# )
	
	bugs <- 'model{
		
		for (i in 1:numSites) {
		
			detects[i] ~ dbin(P[i], effort[i])						# [detection | prob, efforts]
			P[i] <- (p[i]^trueOcc[i]) * (1 - p[i])^(1 - trueOcc[i])	# detectability/occupancy
			p[i] ~ dbeta(1, 1)										# [detectability | . ]
			trueOcc[i] ~ dbern(psi[i])								# [present | prob present]
			psi[i] ~ dbeta(1, 1)									# [prob present | . ]
		
		}
		
	}'

	# MCMC
	jags <- jags.model(
		textConnection(bugs),
		data = data,
		n.chains = chains,
		n.adapt = warmupIters,
		quiet = !verbose
	)
	
	update(jags, n.iter=sampleIters)
	
	posts <- coda.samples(
		jags,
		c('psi', 'p'),
		n.iter=sampleIters,
		thin=thin
	)

	out <- list()
	out$data <- origData
	out$posts <- posts
	out
	
}
