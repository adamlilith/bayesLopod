
spData=stack("C:/GitHub/bayesLopod/simScenario/sim25Sampling_Biased.grd")
rasterN = spData[["sampEff"]]
rastery = spData[["totalDetVarP"]]

LopodObject = rasterLopodData(rasterN, rastery, Adjacency = T)
spplot(LopodObject@geoDataObject)

ModLopod = modelLopod(LopodObject, varP = F, q = 0.01, CAR = T, pmin = 0, nChains = 1, warmup = 20, sampling = 10, nCores = 2)

modelParams(ModLopod)
lopodSummary(ModLopod, probs = NULL)
lopodDens(ModLopod)
spplot(lopodRaster(ModLopod, param="pp", metric="mean", extrapolate = F))
