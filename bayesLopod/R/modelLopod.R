#' Run a  Bayes inference of Landscape Occupancy from Presence-Only Data on Stan
#'
#' @param LopodData Obejct with the data to be used in the Model
#' @param varP Boolean. If TRUE, detectability will vary across cells. If FALSE a global value for detectability will be estimated.
#' @param CAR Boolean. If TRUE, (and if a adjancency matrix is included in the LopodData object) a conditional autoregression analysis will be performed for occupancy across units.
#' @param pmin Number between 0 and 1. Minimum value for detectability in a unit in which the species occurs.
#' @param q Number between 0 and 1 or NULL. Rate of false detections. If NULL the values will be estimated by the model.
#' @param nChains Number of Markov chains used by the Stan model.
#' @param warmup Number of iterations for each chain to be discarted as warm-up.
#' @param sampling Number of iterations for each chain to be sampled (after warm-up).
#' @param nCores Number of cores to use when executing the chains in parallel.
#'
#' @return A LopodModel object.
#' @examples
#' data("simSpRecords", package = "bayesLopod")
#' data("simSpSamplingEffort", package = "bayesLopod")
#' simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 10, extentExpansion = 0)
#' ld_Raster = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = F )
#' mLopodRaster = modelLopod(LopodData = ld_Raster, varP = T, q = NULL, pmin = 0, CAR = F, nChains = 1,warmup = 10,sampling = 10,nCores = 1)
#'
#' data("Andropogon_shape", package = "bayesLopod")
#' ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = F, keepFields = F)
#' mLopodShape = modelLopod(LopodData = ld_Shape, varP = F, q = NULL, pmin = 0, CAR = F, nChains = 1,warmup = 5,sampling = 5,nCores = 1)
#'
#' \dontrun{
#' data("simSpRecords", package = "bayesLopod")
#' data("simSpSamplingEffort", package = "bayesLopod")
#' simSpRasters = xyToRaster(xyRecords = simSpRecords,xySamplingEffort = simSpSamplingEffort,basemap = NULL, nrows = 50, extentExpansion = 0)
#' ld_Raster_adMatrix = rasterLopodData(rasterN = simSpRasters[["samplingEffort"]], rasterY = simSpRasters[["spDetections"]], Adjacency = T )
#' mLopodRaster = modelLopod(LopodData = ld_Raster_adMatrix, varP = T, q = NULL, pmin = 0.1, CAR = F,nChains = 4,warmup = 500,sampling = 100,nCores = 4)
#'
#' data("Andropogon_shape", package = "bayesLopod")
#' ld_Shape = shapeLopodData(Shapefile = Andropogon_shape, fieldN = "sampEffort", fieldY = "detections",  Adjacency = T, keepFields = F)
#' mLopodShape = modelLopod(LopodData = ld_Shape, varP = T, q = NULL, pmin = 0, CAR = T, nChains = 4,warmup = 500,sampling = 100,nCores =4)
#' }

modelLopod = function(LopodData, varP = F, q =  NULL, pmin = 0, CAR = F, nChains = 4, warmup = 2000, sampling = 1000, nCores=4){

  stanFilesDir = "./exec/"

#Check values anda data types

if (class(LopodData) != "LopodData") stop("Data should be entered in a LopodData Object")

if (is.null(q)==F){
  if (q<0 | q>1) stop("q must be NULL or between 0 and 1")
}

if (pmin<0 | pmin>1) stop("pmin must be between 0 and 1")

#Get N and y from different formats

if(LopodData@geoType == "Raster"){

  N = LopodData@geoDataObject[["samplingEffort"]][LopodData@geoInfo$sampledId$cellRaster]
  y = LopodData@geoDataObject[["Detections"]][LopodData@geoInfo$sampledId$cellRaster]

}
if(LopodData@geoType == "Shapefile"){

  N = LopodData@geoDataObject$sampEffort[LopodData@geoInfo$sampledId$featureShape]
  y = LopodData@geoDataObject$detections[LopodData@geoInfo$sampledId$featureShape]

}

  if(LopodData@geoType != "Shapefile"&LopodData@geoType != "Raster"){
    stop("Only Raster or Shapefiles inputs are currently supported")}

  modelInfo = list(varP = varP, q =  q, CAR = CAR)

  #### Stan Files and Stan Data

  if (CAR == F) {

    if (varP == F){

      if (is.null(q)==T){


        if(LopodData@geoType == "Raster"){
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin
          )

        }
        if(LopodData@geoType == "Shapefile"){
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin
          )

        }


        message("Global p and q estimated. Psy for each sampling unit.")
        StanModel = stanmodels$psyipq


      }

      if (is.null(q)==F) {

        if (pmin < q) pmin = q

        if(LopodData@geoType == "Raster"){

          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin,
                           q = q

          )

        }
        if(LopodData@geoType == "Shapefile"){

          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin,
                           q = q

          )

        }


        message("Global p estimated assuming p is larger than the given q (which can be 0, in which case there are no false detections). Psy estimated for each sampling unit.")
        StanModel = stanmodels$psyip

      }
    }

    if (varP == T){

      if (is.null(q)==T){

        if(LopodData@geoType == "Raster"){

          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin
          )

        }
        if(LopodData@geoType == "Shapefile"){

          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin
          )

        }


        message("Global q estimated. Psy and P for each sampling unit")

          StanModel = stanmodels$psyipiq


      }

      if (is.null(q)==F) {

        if (pmin < q) pmin = q

        if(LopodData@geoType == "Raster"){
          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin,
                           q = q
          )



        }
        if(LopodData@geoType == "Shapefile"){

          stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                           sampledId = LopodData@geoInfo$sampledId$cellStan,
                           N = N,
                           y = y,
                           minP = pmin,
                           q = q
          )



        }

        message("Psy and p estimated for each sampling unit assuming p is larger than the given q (which can be 0, in which case there are no false detections).")
        StanModel = stanmodels$psyipi


      }
    }
  }

  if (CAR == T) {
    if(is.null(LopodData@geoInfo$W_sparse)==T){
      stop("Cannot perform CAR analysis without adjacency matrix")
    } else {

      if (varP == F){

        if (is.null(q)==T){

           if(LopodData@geoType == "Raster"){
             stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                              sampledId = LopodData@geoInfo$sampledId$cellStan,
                              nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                              notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                              n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                              W_n = dim(LopodData@geoInfo$W_sparse)[1],
                              W_sparse = LopodData@geoInfo$W_sparse,
                              D_sparse = LopodData@geoInfo$D_sparse,
                              lambda = LopodData@geoInfo$lambda_sparse,
                              N = N,
                              y = y,
                              minP = pmin
             )

          }
          if(LopodData@geoType == "Shapefile"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$featureShape)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin
            )

          }


          message("Global p and q estimated. Psy for each sampling unit. Psy is spatially autocorrelated.")
          StanModel = stanmodels$psyipq_CAR

        }

        if (is.null(q)==F) {

          if (pmin < q) pmin = q

          if(LopodData@geoType == "Raster"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin,
                             q = q
            )


          }
          if(LopodData@geoType == "Shapefile"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$featureShape)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin,
                             q = q
            )


          }



          message("Global p estimated assuming p is larger than the given q (which can be 0, in which case there are no false detections). Psy estimated for each sampling unit. Psy is spatially autocorrelated.")
          StanModel = stanmodels$psyip_CAR

        }
      }

      if (varP == T){

        if (is.null(q)==T){

          if(LopodData@geoType == "Raster"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin
            )


          }

          if(LopodData@geoType == "Shapefile"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$featureShape)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin
            )


          }


          message("Global q estimated. Psy and P for each sampling unit. Psy is spatially autocorrelated.")
          StanModel = stanmodels$psyipiq_CAR

        }

        if (is.null(q)==F) {

          if (pmin < q) pmin = q

          if(LopodData@geoType == "Raster"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$cellRaster),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$cellRaster)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin,
                             q = q
            )


          }
          if(LopodData@geoType == "Shapefile"){

            stanData = list( nSampledCells = length(LopodData@geoInfo$sampledId$featureShape),
                             sampledId = LopodData@geoInfo$sampledId$cellStan,
                             nNotSampled = length(LopodData@geoInfo$notSampledId$cellStan),
                             notSampledId = LopodData@geoInfo$notSampledId$cellStan,
                             n = length(LopodData@geoInfo$sampledId$featureShape)+length(LopodData@geoInfo$notSampledId$cellStan),
                             W_n = dim(LopodData@geoInfo$W_sparse)[1],
                             W_sparse = LopodData@geoInfo$W_sparse,
                             D_sparse = LopodData@geoInfo$D_sparse,
                             lambda = LopodData@geoInfo$lambda_sparse,
                             N = N,
                             y = y,
                             minP = pmin,
                             q = q
            )


          }

          message("Psy and p estimated for each sampling unit assuming p is larger than the given q (which can be 0, in which case there are no false detections). Psy is spatially autocorrelated.")
          StanModel = stanmodels$psyipi_CAR

        }
      }
    }
  }


 modelArgs = list(object = StanModel,
           data = stanData,              # named list of data
           chains = nChains,                   # number of Markov chains
           warmup = warmup,               # number of warmup iterations per chain
           iter = sampling+warmup,                 # total number of iterations per chain
           cores = nCores,                    # number of cores
           refresh = 2,                 # show progress every 'refresh' iterations
           control = list(adapt_delta  = 0.8)
  )

 StanFittetModel = do.call("sampling", args = modelArgs, envir = .GlobalEnv)

  return(LopodModel(LopodData = LopodData, StanFit = StanFittetModel, modelInfo = modelInfo))
}

