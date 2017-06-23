#Install from github

#devtools::install_github("camilosanin/bayesLopod/bayesLopod", dependencies = TRUE)
#library(bayesLopod)

#Load testing rasters (any other can be used)
#data(SimSp25sq, package = "bayesLopod")
#data(SimSp50sq, package = "bayesLopod")
#data(SimSp100sq, package = "bayesLopod")

spXYData = read.csv("./data/Oreothlypis_ruficapillaSp.csv")
samEffXYData = read.csv("./data/summerEvents.csv")

spXYSpatial = SpatialPoints(spXYData[,c("Longitude", "Latitude")])
samEffXYSpatial = SpatialPoints(samEffXYData[,c("Longitude", "Latitude")])

range(spXYData[,c("Longitude")])
range(spXYData[,c("Latitude")])

testExtent = extent(c(-110,-58,18,60))

spCount = xyToRaster(spXYSpatial,samEffXYSpatial,extent=testExtent, nrows=100)

#Create Sampling effort raster object
#rasterN = SimSp25sq[["sampEff"]]
rasterN = spCount[["samplingEffort"]]
#Create detectos raster object
#rasterY = SimSp25sq[["totalDetVarP"]]
rasterY = spCount[["spDetections"]]

# Create lopodObject
LopodObject = rasterLopodData(rasterN, rasterY, Adjacency = T, extDetec = 0.07)
# Plot input data
spplot(LopodObject@geoDataObject)

#Run bayesLopod model (change settings tu run it for longer chains or different settings)
ModLopod = modelLopod(LopodObject, varP = F, q = NULL, CAR = T, pmin = 0, nChains = 1, warmup = 100, sampling = 100, nCores = 2)

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



