library(raster)
library(rstan)
library(emulator)
library(slam)
library(ggplot2)
library(sp)
library(rstantools)

source("./RScripts/rasterLopodData.R")
source("./RScripts/LopodData_class.R")
source("./RScripts/LopodModel_class.R")
source("./RScripts/modelLopod.R")
source("./RScripts/modelParams.R")
source("./RScripts/lopodSummary.R")
source("./RScripts/lopodDens.R")
source("./RScripts/lopodRaster.R")



spData=stack("./simScenario/sim50Sampling_Biased.grd")
rasterN = spData[["sampEff"]]
rastery = spData[["totalDetVarP"]]

LopodObject = rasterLopodData(rasterN, rastery, Adjacency = T)
spplot(LopodObject@geoDataObject)

ModLopod = modelLopod(LopodObject, varP = F, q = 0.01, CAR = T, pmin = 0, nChains = 2, warmup = 20, sampling = 10, nCores = 2)

modelParams(ModLopod)
lopodSummary(ModLopod, probs = NULL)
lopodDens(ModLopod)
spplot(lopodRaster(ModLopod, param="pp", metric="mean", extrapolate = F))

ModLopod@StanFit@model_name
ModLopod@modelInfo
