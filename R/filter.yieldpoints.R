#' @title Update filter values
#' @description `filter.yieldpoints` updates filter values (altering the original dataframe).
#' @param indata TODO
#' @param metric one between f_rangev, f_smv, f_mins, f_rangey, f_stdy, f_rangeq, f_pos
#' @param value value or values to apply (depending on metric)
#' @param inlayer optional: SpatialPolygonsDataFrame of fields (a field "idfield" must be present if byfield is TRUE)
#' @param byfield if FALSE (default), consider data as a unique field; if TRUE, iterate each filter on field "idfield"
#' @param samplesize maximum size of the sample of the original data to work with (default: 100000; if NA: all the points)
#' @import data.table
#' @importFrom data.table setkey
#' @importFrom methods is
#' @importFrom raster compareCRS
#' @importFrom sp coordinates over spTransform
#' @importFrom stats sd
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

filter.yieldpoints <- function(
  indata,
  metric,
  value,
  inlayer = NA,
  byfield = FALSE,
  samplesize = 1E5
) {

  # Check samplesize
  if (is.na(samplesize)) {samplesize <- nrow(indata@data)}
  # Check input data
  if (!is(indata, "yield.data.table")) {
    stop("The input object is not a valid yield.data.table.")
  }

  if (byfield) {
    if (is.null(inlayer$idfield)) {
      stop("The inlayer shapefile must contain a field \"idfield\" with field IDs.")
    }
  }

  # Duplicate indata (so to can use ":=")
  # outdata <- copy(indata)
  outdata <- indata # FIXME this alter original dataset!

  # Apply filter
  if (metric=="rangev") {
    outdata@data[sid<=samplesize & (speed<value[1] | speed>value[2]), f_rangev:=TRUE]
    outdata@data[sid<=samplesize & !(speed<value[1] | speed>value[2]), f_rangev:=FALSE]

  } else if (metric=="smv") {
    outdata_smv <- abs(diff(outdata@data$speed))
    outdata@data[sid<=samplesize & (c(outdata_smv,0)>value | c(0,outdata_smv)>value), f_smv:=TRUE]
    outdata@data[sid<=samplesize & !(c(outdata_smv,0)>value | c(0,outdata_smv)>value), f_smv:=FALSE]

  } else if (metric=="mins") {
    outdata@data[sid<=samplesize & rel_width<value, f_mins:=TRUE]
    outdata@data[sid<=samplesize & rel_width>=value, f_mins:=FALSE]

  } else if (metric=="rangey") {
    outdata@data[sid<=samplesize & (yield<value[1] | yield>value[2]), f_rangey:=TRUE]
    outdata@data[sid<=samplesize & !(yield<value[1] | yield>value[2]), f_rangey:=FALSE]

  } else if (metric=="stdy") {
    if (byfield) {
      setkey(outdata@data,idfield)
      for (sel_field in unique(outdata@data$idfield)) {
        outdata_avg <- outdata@data[idfield==sel_field&sid<=samplesize, mean(yield,na.rm=TRUE)]
        outdata_sd <- outdata@data[idfield==sel_field&sid<=samplesize, sd(yield,na.rm=TRUE)]
        outdata@data[idfield==sel_field&sid<=samplesize & abs(yield-outdata_avg)/outdata_sd>value, f_stdy:=TRUE]
        outdata@data[idfield==sel_field&sid<=samplesize & abs(yield-outdata_avg)/outdata_sd<=value, f_stdy:=FALSE]
      }
    } else {
      outdata_avg <- outdata@data[sid<=samplesize, mean(yield,na.rm=TRUE)]
      outdata_sd <- outdata@data[sid<=samplesize, sd(yield,na.rm=TRUE)]
      outdata@data[sid<=samplesize & abs(yield-outdata_avg)/outdata_sd>value, f_stdy:=TRUE]
      outdata@data[sid<=samplesize & abs(yield-outdata_avg)/outdata_sd<=value, f_stdy:=FALSE]
    }

  } else if (metric=="rangeq") {
    if (byfield) {
      setkey(outdata@data,idfield)
      for (sel_field in unique(outdata@data$idfield)) {
        outdata_rangeq <- outdata@data[idfield==sel_field&sid<=samplesize, quantile(yield,value,na.rm=TRUE)]
        outdata@data[idfield==sel_field&sid<=samplesize & (yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=TRUE]
        outdata@data[idfield==sel_field&sid<=samplesize & !(yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=FALSE]
      }
    } else {
      outdata_rangeq <- outdata@data[sid<=samplesize, quantile(yield,value,na.rm=TRUE)]
      outdata@data[sid<=samplesize & (yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=TRUE]
      outdata@data[sid<=samplesize & !(yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=FALSE]
    }

  } else if (metric=="pos") {
    outdata_sp <- outdata@data[sid<=samplesize,list(lon,lat,idfield)]
    inlayer_buffer <- aeqd.buffer(inlayer, width=-value)
    coordinates(outdata_sp) <- c("lon","lat")
    outdata_sp@proj4string <- outdata@crs
    if (!compareCRS(inlayer_buffer@proj4string,outdata_sp@proj4string)) {
      outdata_sp <- spTransform(outdata_sp,inlayer_buffer@proj4string)
    }
    if (byfield) {
      setkey(outdata@data,idfield)
      for (sel_field in unique(outdata@data$idfield)) {
        sel_inlayer <- inlayer_buffer[inlayer_buffer$idfield==sel_field,]
        outdata_pos <- is.na(over(outdata_sp, sel_inlayer)[,1])
        outdata@data[idfield==sel_field&sid<=samplesize & outdata_pos, f_pos:=TRUE]
        outdata@data[idfield==sel_field&sid<=samplesize & !outdata_pos, f_pos:=FALSE]
      }
    } else {
      outdata_pos <- is.na(over(outdata_sp, inlayer_buffer)[,1])
      outdata@data[sid<=samplesize & outdata_pos, f_pos:=TRUE]
      outdata@data[sid<=samplesize & !outdata_pos, f_pos:=FALSE]
    }

  } else stop("Metric is not recognised.")

  # Update global filter
  outdata@data[sid<=samplesize, filter:=f_rangev|f_smv|f_mins|f_rangey|f_stdy|f_rangeq|f_pos]

  return(outdata)

}


#' @name filter.yieldpoints.reset
#' @rdname filter.yieldpoints
#' @description `filter.yieldpoints.reset` resets all filter values to FALSE (altering the original dataframe).
#' @param filters TODO
#' @import data.table
#' @export

filter.yieldpoints.reset <- function(indata, filters = NA) {
  outdata <- indata # no effect
  if (is.na(filters)) { # if NA, reset all; otherwise, only specified filters
    outdata@data[,c("f_rangev","f_smv","f_mins","f_rangey","f_stdy","f_rangeq","f_pos","filter"):=as.list(rep(FALSE,8))]
  } else {
    # TODO check that "filters" contains only allowed values
    outdata@data[,paste0("f_",filters):=as.list(rep(FALSE,8))]
  }
  return(outdata)
}


#' @name filter.yieldpoints.resample
#' @rdname filter.yieldpoints
#' @description `filter.yieldpoints.resample` resets sampling order (altering the original dataframe).
#' @import data.table
#' @importFrom data.table setkey
#' @export

filter.yieldpoints.resample <- function(indata) {
  outdata <- indata # no effect
  outdata@data[,sid:=sample(sid)]
  setkey(outdata@data,sid)
  return(outdata)
}
