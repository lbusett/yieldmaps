#' @title Update filter values
#' @description `filter.yieldpoints` updates filter values (altering the original dataframe).
#' @param indata TODO
#' @param metric one between f_rangev, f_smv, f_mins, f_rangey, f_stdy, f_rangeq, f_pos
#' @param value value or values to apply (depending on metric)
#' @param inlayer optional: SpatialPolygonsDataFrame of fields (see also `id_fieldname`)
#' @param id_fieldname optional: name of the `inlayer` field containing unique ID of fields (default: "idfield")
#' @param byfield if FALSE (default), consider data as a unique field; if TRUE, iterate each filter on field `id_fieldname``
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
  id_fieldname = "idfield",
  byfield = FALSE,
  samplesize = 1E5
) {

  # Check samplesize
  if (is.na(samplesize)) {samplesize <- nrow(indata@datatable)}
  # Check input data
  if (!is(indata, "yield.data.table")) {
    stop("The input object is not a valid yield.data.table.")
  }

  if (byfield) {
    if (is.null(inlayer@data[,id_fieldname])) {
      stop("The inlayer shapefile must contain a field with field IDs (argument \"id_fieldname\").")
    }
  }

  # Duplicate indata (so to can use ":=")
  # outdata <- copy(indata)
  outdata <- indata # FIXME this alter original dataset!

  # Apply filter
  if (metric=="rangev") {
    outdata@datatable[sid<=samplesize & (speed<value[1] | speed>value[2]), f_rangev:=TRUE]
    outdata@datatable[sid<=samplesize & !(speed<value[1] | speed>value[2]), f_rangev:=FALSE]

  } else if (metric=="smv") {
    outdata_smv <- abs(diff(outdata@datatable$speed))
    outdata@datatable[sid<=samplesize & (c(outdata_smv,0)>value | c(0,outdata_smv)>value), f_smv:=TRUE]
    outdata@datatable[sid<=samplesize & !(c(outdata_smv,0)>value | c(0,outdata_smv)>value), f_smv:=FALSE]

  } else if (metric=="mins") {
    outdata@datatable[sid<=samplesize & rel_width<value, f_mins:=TRUE]
    outdata@datatable[sid<=samplesize & rel_width>=value, f_mins:=FALSE]

  } else if (metric=="rangey") {
    outdata@datatable[sid<=samplesize & (yield<value[1] | yield>value[2]), f_rangey:=TRUE]
    outdata@datatable[sid<=samplesize & !(yield<value[1] | yield>value[2]), f_rangey:=FALSE]

  } else if (metric=="stdy") {
    if (byfield) {
      setkey(outdata@datatable,idfield)
      for (sel_field in unique(outdata@datatable$idfield)) {
        outdata_avg <- outdata@datatable[idfield==sel_field&sid<=samplesize, mean(yield,na.rm=TRUE)]
        outdata_sd <- outdata@datatable[idfield==sel_field&sid<=samplesize, sd(yield,na.rm=TRUE)]
        outdata@datatable[idfield==sel_field&sid<=samplesize & abs(yield-outdata_avg)/outdata_sd>value, f_stdy:=TRUE]
        outdata@datatable[idfield==sel_field&sid<=samplesize & abs(yield-outdata_avg)/outdata_sd<=value, f_stdy:=FALSE]
      }
    } else {
      outdata_avg <- outdata@datatable[sid<=samplesize, mean(yield,na.rm=TRUE)]
      outdata_sd <- outdata@datatable[sid<=samplesize, sd(yield,na.rm=TRUE)]
      outdata@datatable[sid<=samplesize & abs(yield-outdata_avg)/outdata_sd>value, f_stdy:=TRUE]
      outdata@datatable[sid<=samplesize & abs(yield-outdata_avg)/outdata_sd<=value, f_stdy:=FALSE]
    }

  } else if (metric=="rangeq") {
    if (byfield) {
      setkey(outdata@datatable,idfield)
      for (sel_field in unique(outdata@datatable$idfield)) {
        outdata_rangeq <- outdata@datatable[idfield==sel_field&sid<=samplesize, quantile(yield,value,na.rm=TRUE)]
        outdata@datatable[idfield==sel_field&sid<=samplesize & (yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=TRUE]
        outdata@datatable[idfield==sel_field&sid<=samplesize & !(yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=FALSE]
      }
    } else {
      outdata_rangeq <- outdata@datatable[sid<=samplesize, quantile(yield,value,na.rm=TRUE)]
      outdata@datatable[sid<=samplesize & (yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=TRUE]
      outdata@datatable[sid<=samplesize & !(yield<outdata_rangeq[1] | yield>outdata_rangeq[2]), f_rangeq:=FALSE]
    }

  } else if (metric=="pos") {
    outdata_sp <- outdata@datatable[sid<=samplesize,list(lon,lat,idfield)]
    inlayer_buffer <- aeqd.buffer(inlayer, width=-value)
    sp::coordinates(outdata_sp) <- c("lon","lat")
    outdata_sp@proj4string <- outdata@crs
    if (!compareCRS(inlayer_buffer@proj4string,outdata_sp@proj4string)) {
      outdata_sp <- spTransform(outdata_sp,inlayer_buffer@proj4string)
    }
    if (byfield) {
      setkey(outdata@datatable,idfield)
      for (sel_field in unique(outdata@datatable$idfield)) {
        sel_inlayer <- inlayer_buffer[inlayer_buffer@data[,id_fieldname]==sel_field,]
        if (nrow(sel_inlayer) > 0) {
          outdata_pos <- is.na(over(outdata_sp, sel_inlayer)[,1])
          outdata@datatable[idfield==sel_field&sid<=samplesize & outdata_pos, f_pos:=TRUE]
          outdata@datatable[idfield==sel_field&sid<=samplesize & !outdata_pos, f_pos:=FALSE]
        } else {
          outdata@datatable[idfield==sel_field&sid<=samplesize, f_pos:=TRUE]
        }
      }
    } else {
      outdata_pos <- is.na(over(outdata_sp, inlayer_buffer)[,1])
      outdata@datatable[sid<=samplesize & outdata_pos, f_pos:=TRUE]
      outdata@datatable[sid<=samplesize & !outdata_pos, f_pos:=FALSE]
    }

  } else stop("Metric is not recognised.")

  # Update global filter
  outdata@datatable[sid<=samplesize, filter:=f_rangev|f_smv|f_mins|f_rangey|f_stdy|f_rangeq|f_pos]

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
    outdata@datatable[,c("f_rangev","f_smv","f_mins","f_rangey","f_stdy","f_rangeq","f_pos","filter"):=as.list(rep(FALSE,8))]
  } else {
    # TODO check that "filters" contains only allowed values
    outdata@datatable[,paste0("f_",filters):=as.list(FALSE)]
    outdata@datatable[,filter:=f_rangev|f_smv|f_mins|f_rangey|f_stdy|f_rangeq|f_pos]
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
  outdata@datatable[,sid:=sample(sid)]
  setkey(outdata@datatable,sid)
  return(outdata)
}
