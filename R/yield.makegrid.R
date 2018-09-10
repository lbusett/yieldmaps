#' @title Create global output grid
#' @description TODO
#' @param indata_sp TODO all input points
#' @param outres `numeric (1)` TODO spatial resolution of output raster (in crs unit)
#' @param border `numeric (1)` TODO extent of border (in crs units)
#' @param outcrs `CRS` output CRS
#' @importFrom sp bbox GridTopology SpatialGrid spTransform
#' @importFrom raster compareCRS
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

# FIXME border should be retrieve automatically from the filters (i.e. to prevent that a Gaussian would cut too much border)
yield.makegrid <- function(
  indata_sp,
  outres = 5,
  border = 15,
  outcrs = indata_sp@proj4string
) {

  # reproj if necessary
  if (!compareCRS(outcrs,indata_sp@proj4string)) {
    indata_sp <- spTransform(indata_sp,outcrs)
  }

  # create global grid
  grdtop = GridTopology(
    cellcentre.offset = bbox(indata_sp)[,1] - rep(border,2),
    cellsize = rep(outres,2),
    cells.dim = ceiling((bbox(indata_sp)[,2]+rep(border,2) - rowMeans(bbox(indata_sp)))/outres*2))
  yield.grd <- SpatialGrid(grdtop, outcrs)

  return(yield.grd)

}
