#' @title Make SPDF from filtered yield data
#' @description TODO
#' @param indata TODO
#' @param outcrs `CRS` output CRS
#' @importFrom sp coordinates CRS spTransform
#' @importFrom raster compareCRS
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

yield.makespdf <- function(
  indata,
  outcrs = CRS("+init=epsg:3035")
) {
  # create SPDF from points
  indata_sp <- indata@data[filter==FALSE,list(sid,idfield,crop,year,yield)]
  coordinates(indata_sp) <- indata@data[filter==FALSE,list(lon,lat)]
  indata_sp@proj4string <- indata@crs
  indata_sp$sid <- rank(indata_sp$sid)
  if (!compareCRS(outcrs,indata_sp@proj4string)) {
    indata_sp <- spTransform(indata_sp,outcrs)
  }
  return(indata_sp)
}
