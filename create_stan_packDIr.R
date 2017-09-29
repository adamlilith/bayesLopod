library(rstantools)
library(raster)
library(rstan)

rFiles = list.files("C:/GitHub/bayesLopod/OriginalData/R", full.names = T)
stanFiles = list.files("C:/GitHub/bayesLopod/OriginalData/R", full.names = T, pattern = ".stan")

rstan_package_skeleton(name = "bayesLopod",   code_files = rFiles, stan_files = stanFiles, path = "C:/GitHub/bayesLopod" )

