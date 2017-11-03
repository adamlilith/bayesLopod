#Install from github

#devtools::install_github("camilosanin/bayesLopod/bayesLopod", dependencies = TRUE)
library(rstan)
options(mc.cores = parallel::detectCores())

#LOAD SIMULATED DATA
SimSp25sq = stack("c:/github/bayeslopod/simscenario/sim50Sampling_biased.grd")
spplot(stack(SimSp25sq[["spOcc"]], SimSp25sq[["sampEff"]]/max(SimSp25sq[["sampEff"]][])))


#Create Sampling effort raster object
rasterN = SimSp25sq[["sampEff"]]

#Create detectos raster object
rasterY = SimSp25sq[["totalDetConstP"]]

# Create lopodObject
LopodObject = rasterLopodData(rasterN, rasterY, Adjacency = T, extSample = 1, extDetection = 1)
str(LopodObject, max.level = 3)

# Plot input data
spplot(LopodObject@geoDataObject)

#Run bayesLopod model
ModLopod = modelLopod(LopodObject, varP = F, q = NULL, CAR = T, pmin = 0, nChains = 4, warmup = 50, sampling = 25, nCores = 4)
str(ModLopod, max.level = 3)


#What are the parameters calculated in this models?
modelParams(ModLopod)
stan_trace(ModLopod@StanFit, pars = "lp__", inc_warmup = T)

#Summary statistics for global parameters
lopodSummary(ModLopod, probs = c(0.25,0.5,0.75))

#Plot kernel od global parameters
lopodDens(ModLopod)

#Create a raster with the posterior probability of presence given a sampling pattern
meanPPPlot=lopodRaster(ModLopod, param="pp", metric="mean", extrapolate = F)

#Create a raster with the likelihood of presence (Psy)
meanPsyPlot=lopodRaster(ModLopod, param="psy_i", metric="mean", extrapolate = T)


spplot(stack(meanPsyPlot,meanPPPlot,SimSp25sq[["spOcc"]]) )


