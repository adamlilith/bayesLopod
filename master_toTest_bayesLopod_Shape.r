#Install from github

#devtools::install_github("camilosanin/bayesLopod/bayesLopod", dependencies = TRUE)
l#ibrary(bayesLopod)

#Load testing rasters (any other can be used)
AndroShape = shapefile("./data/Andro_US_Shape.shp")
AndroShape@data[,"Sampling"] = AndroShape@data[,"poaRec"] + AndroShape@data[,"anyAg1to3"]

# Create lopodObject
LopodObject = shapeLopodData(AndroShape, fieldN = "Sampling", fieldY = "anyAg1to3", Adjacency = T, keepFields = F)
# Plot input data
spplot(LopodObject@geoDataObject)

#Run bayesLopod model (change settings tu run it for longer chains or different settings)
ModLopod = modelLopod(LopodObject, varP = F, q = NULL, CAR = T, pmin = 0, nChains = 1, warmup = 20, sampling = 10, nCores = 2)

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



