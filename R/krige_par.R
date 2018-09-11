#' @title Parallelise kriging
#' @description TODO
#' @param formula TODO
#' @param locations TODO
#' @param newdata TODO
#' @param model TODO
#' @param n_cores TODO
#' @param method TODO
#' @param nmax TODO
#' @param maxdist TODO
#' @importFrom maptools spRbind
#' @importFrom parallel clusterEvalQ clusterExport detectCores makeCluster parLapply stopCluster
#' @importFrom sp coordinates SpatialPixelsDataFrame
#' @importFrom gstat idw krige
#' @importFrom stats as.formula
#' @importFrom methods is
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note Based on https://gis.stackexchange.com/questions/237672/how-to-achieve-parallel-kriging-in-r-to-speed-up-the-process
#' @note License: GPL 3.0

krige_par <- function(
  formula,
  locations,
  newdata,
  model = NA,
  n_cores = NA,
  method = "krige",
  nmax = Inf,
  maxdist = Inf
  ) {

  # check that modl is defined f method is krige
  if (method=="krige" & anyNA(model)) {
    stop("Argument 'model' is mandatory with method=\"krige\".")
  }

  # set default n_cores value
  if (is.na(n_cores)) {
    n_cores <- if (nrow(locations)<1000) {
      1
    } else {
      min(parallel::detectCores()-1, 8) # use at most 8 cores
    }
  }
  if (!is(n_cores,"numeric") | n_cores < 1) {
    warning("Invalid 'n_cores' value; using a default value.")
    n_cores <- min(parallel::detectCores()-1, 8) # use at most 8 cores
  } else {
    n_cores <- as.integer(n_cores)
  }

  # check if overlapping points are present
  if (sum(duplicated(coordinates(locations)))>0) {
    warning("Some overlapping points are present; only the first one will be considered.")
    locations <- locations[!duplicated(coordinates(locations)),]
  }

  # run singlecore if n_cores==1, multicore elsewhere
  if (as.integer(n_cores)==1) {

    # singlecore
    if (method=="krige") {
      krige(as.formula(formula), locations, newdata, model, nmax=nmax, maxdist=maxdist)
    } else if (method=="idw") {
      idw(as.formula(formula), locations, newdata, nmax=nmax, maxdist=maxdist)
    } else {
      stop(paste0("The method \"",method,"\" is not recognised."))
    }

  } else {

    # multicore

    # Initiate cluster (after loading all the necessary object to R environment: meuse, meuse.grid, m)

    newdata2 <- SpatialPixelsDataFrame(
      newdata,
      data = data.frame("na"=rep(NA,length(newdata))),
      proj4string = newdata@proj4string
    )
    cl <- parallel::makeCluster(n_cores)
    parts <- split(x = 1:nrow(newdata2), f = rep_len(1:n_cores,nrow(newdata2)))
    clusterExport(
      cl = cl,
      varlist = c("locations", "newdata", "model", "parts", "method", "nmax", "maxdist"),
      envir = environment()
    )
    clusterEvalQ(cl = cl, expr = c(library(sp), library(gstat)))
    parallelX <- parLapply(
      cl = cl,
      X = 1:n_cores,
      fun = function(x) {
        if (method=="krige") {
          krige(as.formula(formula), locations, newdata2[parts[[x]],], model, nmax=nmax, maxdist=maxdist)
        } else if (method=="idw") {
          idw(as.formula(formula), locations, newdata2[parts[[x]],], nmax=nmax, maxdist=maxdist)
        } else {
          stop(paste0("The method \"",method,"\" is not recognised."))
        }

      }
    )
    parallel::stopCluster(cl)

    # Merge all the predictions
    mergeParallelX <- spRbind(parallelX[[1]], parallelX[[2]])
    for (i in seq_along(parallelX)[-c(1:2)]) {
      mergeParallelX <- spRbind(mergeParallelX, parallelX[[i]])
    }

    # Create SpatialPixelsDataFrame from mergeParallelX
    SpatialPixelsDataFrame(points = mergeParallelX, data = mergeParallelX@data)

  }

}
