# compiling bayesLopod

# Adam's computer only:
.libPaths('C:/ecology/libraries')

# to ensure compilation (otherwise just runs as-is and slower)
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native",
    if( grepl("^darwin", R.version$os)) "CXX14FLAGS += -arch x86_64 -ftemplate-depth-256" else 
    if (.Platform$OS.type == "windows") "CXX11FLAGS=-O3 -march=native -mtune=native" else
    "CXX14FLAGS += -fPIC",
    file = M, sep = "\n", append = TRUE)

library(sp)
library(raster)
	
files <- list.files('C:/Ecology/Drive/R/bayesLopod/R')
for (f in ff) source(ff)

devtools::document()

# then compile using RStudio menu
