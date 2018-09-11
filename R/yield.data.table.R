#' @title Class yield.data.table
#' @name yield.data.table-class
#' @rdname yield.data.table
#' @description `yield.data.table` is an R class for yield data.
#' @slot datatable `data.table` TODO
#' @slot comm_humid `numeric (1)` TODO
#' @slot max_width `numeric (1)` TODO
#' @slot time_step `numeric (1)` TODO
#' @slot crs `CRS` TODO
#' @slot format `character (1)` TODO
#' @import data.table
#' @importFrom sp CRS
#' @importFrom methods is setClass
#' @exportClass yield.data.table
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

yield.data.table <- setClass(
  "yield.data.table",
  # contains = "data.table",
  slots = c(
    datatable="data.table",
    comm_humid="numeric",
    max_width="numeric",
    time_step="numeric",
    crs="CRS",
    format="character"
  ),
  validity = function(object) {
    if (!is.data.table(object@data))
      return("data slot is not a data.table")
    # FIXME dd check on colnames and filters
    if (!is.numeric(object@comm_humid))
      return("comm_humid slot is not a numeric")
    if (!is.numeric(object@max_width))
      return("max_width slot is not a numeric")
    if (!is.numeric(object@time_step))
      return("time_step slot is not a numeric")
    if (!is(object@crs,"CRS"))
      return("crs slot is not a CRS")
    if (!is.character(object@format))
      return("format slot is not a numeric")
    # if (!is.data.table(object@filters) & !is.null(object@filters))
    #   return("filters slot is not a data.table")
    return(TRUE)
  }
)


#' @name make.yield.data.table
#' @rdname yield.data.table
#' @description `make.yield.data.table` creates a new `yield.data.table` object.
#' @param datatable `data.table` TODO
#' @param comm_humid `numeric (1)` TODO
#' @param max_width `numeric (1)` TODO
#' @param time_step `numeric (1)` TODO
#' @param crs `CRS` TODO
#' @param format `character (1)` TODO
#' @importFrom sp CRS
#' @importFrom methods is setClass
#' @export

make.yield.data.table <- function(
  datatable,
  comm_humid = as.numeric(NA),
  max_width = as.numeric(NA),
  time_step = as.numeric(NA),
  crs = CRS(as.character(NA)),
  format = as.character(NA)
) {
  new(
    "yield.data.table",
    data = datatable,
    comm_humid = comm_humid,
    max_width = max_width,
    time_step = time_step,
    crs = crs,
    format = format
  )
}
