#library(bayesLopod)

## Create and save spatialPoints Objects

samplingEffort_df = read.csv("c:/GitHub/bayesLopod/Data/sampleEffort.csv")
records_df = read.csv("c:/GitHub/bayesLopod/Data/Records.csv")

simSpSamplingEffort = SpatialPoints(samplingEffort_df)
simSpRecords = SpatialPoints(records_df)

devtools::use_data(simSpSamplingEffort)
devtools::use_data(simSpRecords)

## Create a Raster ModelLopod example
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)
ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = T )
mLopodRaster = modelLopod(LopodData = ld_Raster_adMatrix, varP = F, q = NULL, pmin = 0.1, CAR = T,nChains = 1,warmup = 25,sampling = 20,nCores = 1)
devtools::use_data(mLopodRaster, overwrite = F)

lopodTrace(mLopodRaster, inc_warmup = T)


### Create Shape DAta
load("c:/GitHub/bayesLopod/Data/Andropogon_shape.rda")

devtools::use_data(Andropogon_shape)

## Create a Shape ModelLopod example
data("Andropogon_shape", package = "bayesLopod")
ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = T, keepFields = F)
mLopodShape = modelLopod(LopodData = ld_Shape, varP = T, q = NULL, pmin = 0, CAR = T, nChains = 4,warmup = 100,sampling = 20,nCores =4)

devtools::use_data(mLopodRaster, overwrite = F)

lopodTrace(mLopodRaster, inc_warmup = T)

