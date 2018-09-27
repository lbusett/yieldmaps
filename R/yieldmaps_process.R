#' @title Process a group of fields
#' @description This function calls other functions of the library to perform
#'  interpolation over different fields.
#' @param rese_input TODO
#' @param inlayer SpatialPolygonsDataFrame of fields (see also `id_fieldname`)
#' @param grid_path optional: TODO
#' @param filtered logical: is `rese_input` already filtered? If TRUE (default),
#'  no additional filter is applied; if FALSE, a standard automatic filter is applied.
#' @param id_fieldname optional: name of the `inlayer` field containing unique ID of fields (default: "idfield")
#' @param interp_dir directory where rasters are stores (default: temporary directory)
#' @param out_crs optional: CRS of output raster (default: CRS of rese_input)
#' @param samplesize maximum size of the sample of the original data to work with (default: 10000; if NA: all the points)
#' @param buffer_radius numeric: buffer (default: 15) to be applied internally to field borders.
#' @param vgm named list of variograms (names must correspond to values of field `id_fieldname`).
#'  If NA (default), they are automatically computed.
#' @param samplesize maximum size of the sample of the original data to work with (default: 100000; if NA: all the points)
#' @import data.table
#' @importFrom gstat vgm fit.variogram variogram
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach "%do%" "%dopar%"
#' @importFrom raster crop values writeRaster
#' @importFrom rgdal readGDAL
#' @importFrom rgeos gBuffer gIntersection
#' @importFrom sp spTransform
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


yieldmaps_process <- function(
  rese_input,
  inlayer,
  grid_path = NA,
  filtered = TRUE,
  id_fieldname="idfield",
  interp_dir = tempfile(),
  out_crs = NA,
  samplesize = 1E4,
  buffer_radius = 15,
  vgm = NA
) {

  ## Autofilter if not already filtered (filtered == FALSE)
  rese_input_f <- if (filtered == FALSE) {
    filter.yieldpoints(
      rese_input, "rangeq", c(.02,.98), # filter points < 2° and > 98° percentiles
      inlayer = inlayer,
      byfield = TRUE, samplesize = NA
    )
  } else {rese_input}

  # internal conversion
  if (is.na(out_crs)) {out_crs <- rese_input@crs}
  rese_sp <- yield.makespdf(rese_input_f, outcrs = out_crs)

  ## Auto compute variograms if not provided
  if (!is.list(vgm)) {
    vgm <- list()
    vgm_man <- vgm(psill=1, model="Exp", range=100, nugget=0)
    for (f in unique(rese_input_f@datatable$idfield)) {
      v <- variogram(
        yield ~ 1,
        rese_sp[rese_sp$idfield==f,],
        cutoff = round(sqrt(sum(apply(rese_sp[rese_sp$idfield==f,]@bbox,1,diff)^2))/3)
      )
      vgm[[f]] <- fit.variogram(v, vgm_man, fit.sills=TRUE, fit.ranges=TRUE)
    }
  } else {
    # TODO controlla che vgm sia una named list, e che i nomi corrispondano ai livelli di idfield
  }

  # Ritaglio sulle aree dove ci sono dati
  n_cores <- min(parallel::detectCores()-1, length(unique(rese_sp$idfield)), 8) # use at most 8 cores
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  fieldbuffers_step3 <- foreach(f = unique(rese_sp$idfield), .combine=raster::bind, .packages=c("rgeos","sp","yieldmaps")) %dopar% {
    if (sum(rese_sp$idfield==f)>0) {
      sel_rese_sp_b <- yieldmaps:::buffer.f(rese_sp[rese_sp$idfield==f,], buffer_radius/2, 1) # rarefy points by distance
      sel_rese_sp_b1 <- gBuffer(sel_rese_sp_b, width=buffer_radius+10, byid=FALSE) # overbuffer (10m)
      sel_rese_sp_b2 <- gBuffer(sel_rese_sp_b1, width=-10, byid=FALSE) # underbuffer (-10m)
      # intersect buffer with field boundaries
      sel_rese_sp_b3 <- gIntersection(
        sel_rese_sp_b2,
        spTransform(inlayer[inlayer@data[,id_fieldname]==f,], sel_rese_sp_b2@proj4string)
      )
      sel_rese_sp_b3
    } else {
      SpatialPolygons(list())
    }
  }
  stopCluster(cl)
  # crop polygons on buffers
  fieldbuffers <- crop(inlayer, spTransform(fieldbuffers_step3, inlayer@proj4string))


  ## Interpola
  glob_grid <- if (!is.na(grid_path)) {readGDAL(grid_path)} else {yield.makegrid(rese_sp)}

  rasters_list <- foreach(f = unique(rese_sp$idfield)) %do% {
    field_name <- f
    if (!file.exists(file.path(interp_dir,"focal_Gauss5x5",paste0(field_name,"_5m.tif")))) { # this will result in not overwriting existing files
      print(paste(Sys.time(), field_name))
      interp.yieldpoints(
        rese_sp[rese_sp$idfield==f,],
        fieldbuffers[fieldbuffers$id_geom==field_name,],
        vgm[[f]],
        glob_grid,
        border = 0,
        # focal_types = rep("Gauss",2),
        # focal_d = list(5,9),
        focal_types = "Gauss",
        focal_d = 5,
        outname = paste0(field_name,"_5m.tif"),
        outdir = interp_dir,
        n_cores = 16,
        samplesize = samplesize,
        nmax = 250#,
        # maxdist = vgm[[year]][[i]][2,"range"]
      )
    }
    file.path(interp_dir, "focal_Gauss5x5", paste0(field_name,"_5m.tif")) # existing files will be also considered for merging, in this way
    # FIXME: edit interp.yieldpoints to return raster in a specified folder
  }


  # merge fields
  rasters <- lapply(rasters_list, raster)

  # exclude automatically rasters with too high values
  rasters_avg <- lapply(rasters, function(x){mean(values(x),na.rm=TRUE)})
  rasters_filtered <- rasters[rasters_avg<30]
  rasters_excluded <- rasters[rasters_avg>=30]

  merged <- if (length(rasters_filtered) > 1) {
    do.call(raster::merge, rasters_filtered)
  } else {
    rasters_filtered[[1]]
  }
  outname <- paste0("rese_",strftime(Sys.time(), "%Y%m%d%H%M%S"),".tif")
  writeRaster(
    merged,
    file.path(dirname(interp_dir),outname),
    options=c("COMPRESS=DEFLATE"),
    NAflag = -32768,
    overwrite = TRUE
  )

  return(file.path(dirname(interp_dir),outname))

}
