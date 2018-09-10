#' @title Mosaic field tiffs
#' @description TODO
#' @param indir the directory in which field maps are stored (equivalent of outdir+subdir in interp.yieldpoints())
#' @param reg_ex `character (1)` optional regular expression to be applied of indir content
#' @param outfile output raster file (with path)
#' @importFrom raster writeRaster
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

merge.yieldmaps <- function(
  indir,
  reg_ex = "\\.tif$",
  outfile
) {

  # load input tiffs
  input_names <- list.files(indir,reg_ex)
  input_tiffs <- lapply(file.path(indir,input_names),raster)

  # mosaic
  input_tiffs$fun<- mean
  out_merged <- do.call(mosaic, input_tiffs)

  # write output
  writeRaster(out_merged,outfile, options='COMPRESS=DEFLATE')

}
