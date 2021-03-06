% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interp.yieldpoints.R
\name{interp.yieldpoints}
\alias{interp.yieldpoints}
\title{Interpolate yield points of a single field}
\usage{
interp.yieldpoints(indata_sp, field_poly, vgm.fit, globgrid, method = "krige",
  n_cores = 1, focal_types = NULL, focal_d = NULL, border = 0,
  outdir = tempdir(), outname = NA, samplesize = 10000, nmax = 1000,
  maxdist = Inf)
}
\arguments{
\item{indata_sp}{points of a single field (with optional "idfield" attribute)}

\item{field_poly}{polygon of the field}

\item{vgm.fit}{variogram}

\item{globgrid}{grid generated by yield.makegrid()}

\item{method}{one between "krige" (default) and "idw"}

\item{n_cores}{1 for singlecore (default), more for multicore, NA for multicore autodetection}

\item{focal_types}{smoothing filters to be applied (vector of types of focalWeight())}

\item{focal_d}{list of filter parameters (see argument d of focalWeight())}

\item{border}{border (in crs unit) to be cut (default: 0)}

\item{outdir}{directory where rasters are stores (default: temporary directory)}

\item{outname}{name of the output tif}

\item{samplesize}{maximum size of the sample of the original data to work with (default: 10000; if NA: all the points)}

\item{nmax}{argument of krige() and idw()}

\item{maxdist}{argument of krige() and idw()}
}
\description{
TODO
}
\note{
License: GPL 3.0
}
\author{
Luigi Ranghetti, phD (2018) \email{ranghetti.l@irea.cnr.it}
}
