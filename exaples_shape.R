
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

#lopodTrace
#Run:

data("mLopodShape", package = "bayesLopod")
lopodTrace(mLopodShape, inc_warmup = T)

#lopodDens
#Run:

data("mLopodShape", package = "bayesLopod")
lopodDens(mLopodShape, c("q", "pmin", "pmax"))


#lopodSummary
#Run:

data("mLopodShape", package = "bayesLopod")
lopodSummary(mLopodRaster)

#lopodShape
#Run:

data("mLopodShape", package = "bayesLopod")
psyShape = lopodShape(mLopodShape, "psy_i", extrapolate = F,  quant = 0.95)
spplot(psyShape, zcol = "psy_i")


data("mLopodShape", package = "bayesLopod")
psyShape = lopodShape(mLopodShape, "psy_i", extrapolate = T, quant = 0.05)
spplot(psyShape, zcol = "psy_i")

