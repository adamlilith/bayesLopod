library(raster)
library(rstan)
library(emulator)
library(slam)
library(ggplot2)

source("./RScripts/rasterLopodData.R")
source("./RScripts/LopodData_class.R")
source("./RScripts/LopodModel_class.R")
source("./RScripts/modelLopod.R")


spData=stack("./simScenario/sim50Sampling_Biased.grd")
rasterN = spData[["sampEff"]]
rastery = spData[["totalDetVarP"]]

LopodObject = rasterLopodData(rasterN, rastery, Adjacency = T)
spplot(LopodObject@geoDataObject)

ModLopod = modelLopod(LopodObject, varP = F, q = NULL, CAR = T, pmin = 0, nChains = 2, warmup = 20, sampling = 10, nCores = 2)

ModLopod@StanFit@model_name
ModLopod@modelInfo
