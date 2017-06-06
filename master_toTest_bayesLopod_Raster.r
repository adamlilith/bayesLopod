#Install from github

#devtools::install_github("camilosanin/bayesLopod/bayesLopod", dependencies = TRUE)
#library(bayesLopod)

#Load testing rasters (any other can be used)
data(SimSp25sq, package = "bayesLopod")
data(SimSp50sq, package = "bayesLopod")
data(SimSp100sq, package = "bayesLopod")

#Create Sampling effort raster object
rasterN = SimSp25sq[["sampEff"]]

#Create detectos raster object
rasterY = SimSp25sq[["totalDetVarP"]]

# Create lopodObject
LopodObject = rasterLopodData(rasterN, rasterY, Adjacency = T)
# Plot input data
spplot(LopodObject@geoDataObject)

#Run bayesLopod model (change settings tu run it for longer chains or different settings)
ModLopod = modelLopod(LopodObject, varP = F, q = NULL, CAR = F, pmin = 0, nChains = 1, warmup = 20, sampling = 10, nCores = 2)

#What are the parameters calculated in this models?
modelParams(ModLopod)

#Summary statistics for global parameters
lopodSummary(ModLopod, probs = c(0.25,0.5,0.75))

#Plot kernel od global parameters
lopodDens(ModLopod)

#Create a raster with the mean probability of presence (change parameters for other)
meanPPPlot=lopodRaster(ModLopod, param="pp", metric="mean", extrapolate = T)

#plot raster
spplot(meanPPPlot)



