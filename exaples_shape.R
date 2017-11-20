
#shapeLopodData

#Run:
data("Andropogon_shape", package = "bayesLopod")
ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = F, keepFields = F)

#Do not Run:
data("Andropogon_shape", package = "bayesLopod")
ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = T, keepFields = F)

#modelLopod

#Run:
data("Andropogon_shape", package = "bayesLopod")
ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = F, keepFields = F)
mLopodShape = modelLopod(LopodData = ld_Shape, varP = F, q = NULL, pmin = 0, CAR = F, nChains = 1,warmup = 5,sampling = 5,nCores = 1)

#Do not Run:
data("Andropogon_shape", package = "bayesLopod")
ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = T, keepFields = F)
mLopodShape = modelLopod(LopodData = ld_Shape, varP = T, q = NULL, pmin = 0, CAR = T, nChains = 4,warmup = 500,sampling = 100,nCores =4)

#Do not Run:
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)
ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = T )
mLopodRaster = modelLopod(LopodData = ld_Raster_adMatrix, varP = T, q = NULL, pmin = 0.1, CAR = F,nChains = 4,warmup = 500,sampling = 100,nCores = 4)

#lopodTrace
#Run:

data("mLopodRaster", package = "bayesLopod")
lopodTrace(mLopodRaster, inc_warmup = T)

#lopodDens
#Run:

data("mLopodRaster", package = "bayesLopod")
lopodDens(mLopodRaster)


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
