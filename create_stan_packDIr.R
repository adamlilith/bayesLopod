library(rstantools)


rFiles = list.files("./RScripts", full.names = T)
stanFiles = list.files("./StanFiles", full.names = T, pattern = ".stan")

rstan_package_skeleton(name = "bayesLopod",   code_files = rFiles, stan_files = stanFiles, path = "C:/GitHub/Packages/bayesLopod" )
