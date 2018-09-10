#' @title Buffer latlon polygons in metres
#' @description TODO
#' @param poly TODO
#' @param width TODO
#' @importFrom rgeos gBuffer gCentroid
#' @importFrom sp CRS is.projected spTransform
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note Modified from https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world
#' @note License: GPL 3.0

aeqd.buffer <- function(poly, width) {
  p <- gCentroid(poly,byid=FALSE)
  projected <- if (!is.projected(p)) {
    aeqd <- sprintf("+proj=aeqd +lat_0=%s +lon_0=%s +x_0=0 +y_0=0",
                    p@coords[[2]], p@coords[[1]])
    spTransform(poly, CRS(aeqd))
  } else {
    poly
  }
  buffered <- gBuffer(projected, width=width, byid=TRUE)
  spTransform(buffered, poly@proj4string)
}
