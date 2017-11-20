#Load spatial points objects for Simulated species
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")

#xyToRaster
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)

#rasterLopodData

#Run:
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 10, extentExpansion = 0)
ld_Raster = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = F )

#Do not Run:
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)
ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = T )

#modelLopod

#Run:
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 10, extentExpansion = 0)
ld_Raster = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = F )
mLopodRaster = modelLopod(LopodData = ld_Raster, varP = T, q = NULL, pmin = 0, CAR = F, nChains = 1,warmup = 10,sampling = 10,nCores = 1)

#Do not Run:
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)
ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = T )
mLopodRaster = modelLopod(LopodData = ld_Raster_adMatrix, varP = T, q = NULL, pmin = 0.1, CAR = F,nChains = 4,warmup = 500,sampling = 100,nCores = 4)

#lopodTrace
#Run:

data("mLopodRaster", package = "bayesLopod")
lopodTrace(mLopodRaster, inc_warmup = F, params = c("p","q"))

#lopodDens
#Run:

data("mLopodRaster", package = "bayesLopod")
lopodDens(mLopodRaster, c("alpha", "tau"))


#lopodSummary
#Run:

data("mLopodRaster", package = "bayesLopod")
lopodSummary(mLopodRaster, params = c("psy","p","q"))

#lopodRaster
#Run:

data("mLopodRaster", package = "bayesLopod")
psyRaster = lopodRaster(mLopodRaster, param = "psy_i", extrapolate = T, quant = 0.5)
spplot(psyRaster)

data("mLopodRaster", package = "bayesLopod")
ppRaster = lopodRaster(mLopodRaster, param = "pp", extrapolate = F, metric = "mean")
spplot(ppRaster)
