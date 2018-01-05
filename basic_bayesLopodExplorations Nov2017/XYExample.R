library(raster)
raster = stack("C:/github/bayeslopod/simscenario/simsampling.grd")
plot(raster)

raster[sample(1:dim(raster[])[1], size = dim(raster[])[1]*0.90, replace = F)] = NA

Records = raster[["totalDetConstP.1"]]/raster[["sampEff"]]
Records[is.na(Records[])] = 0
Records[Records[] <= 0.1] = 0
Records[Records[] > 0.1] = 1
plot(Records)

Sampling = raster[["sampEff"]]
Sampling[is.na(Sampling[])] = 0
Sampling[Sampling[] > 0] = 1
plot(Sampling)


#write.csv(floor(xyFromCell(Sampling,which(Sampling[] == 1))*10)/10,"c:/GitHub/bayesLopod/Data/sampleEffort.csv", row.names = F)
#write.csv(floor(xyFromCell(Records,which(Records[] == 1))*10)/10,"c:/GitHub/bayesLopod/Data/Records.csv", row.names = F)


samplingEffort = SpatialPoints(read.csv("c:/GitHub/bayesLopod/Data/sampleEffort.csv"))
records = SpatialPoints(read.csv("c:/GitHub/bayesLopod/Data/Records.csv"))
save("samplingEffort", file = "c:/GitHub/bayesLopod/Data/samplingEffort.rda")
save("records", file = "c:/GitHub/bayesLopod/Data/records.rda")


Rasters = xyToRaster(xyRecords = records,xySamplingEffort = samplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)

#Create Sampling effort raster object
rasterN = Rasters[["samplingEffort"]]

#Create detectos raster object
rasterY = Rasters[["spDetections"]]

# Create lopodObject
LopodObject = rasterLopodData(rasterN, rasterY, Adjacency = T)
# Plot input data
spplot(LopodObject@geoDataObject)

#Run bayesLopod model (change settings tu run it for longer chains or different settings)
# ModLopod = modelLopod(LopodObject, varP = F, q = NULL, CAR = F, pmin = 0, nChains = 4, warmup = 900, sampling = 100, nCores = 4)
ModLopod = modelLopod(LopodObject, varP = T, q = NULL, CAR = T, pmin = 0, nChains = 1, warmup = 200, sampling = 100, nCores = 2)
lopodTrace(ModLopod)
#What are the parameters calculated in this models?
modelParams(ModLopod)

#Summary statistics for global parameters
lopodSummary(ModLopod, probs = c(0.25,0.5,0.75))

#Plot kernel od global parameters
lopodDens(ModLopod)

#Create a raster with the mean probability of presence (change parameters for other)
meanPPPlot=lopodRaster(ModLopod, param="psy_Sampled", metric="mean", extrapolate = F)

#plot raster
spplot(meanPPPlot)



