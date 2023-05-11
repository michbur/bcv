#' Cross-Validation for the SVD (Bi-Cross-Validation)
#' 
#' This package implements methods for choosing the rank of an SVD
#' approximation via cross validation.  It provides both Gabriel-style "block"
#' holdouts and Wold-style "speckled" holdouts.  Also included is an
#' implementation of the SVDImpute algorithm.
#' 
#' \tabular{ll}{ Package: \tab bcv\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2009-08-15\cr License: \tab BSD3\cr }
#' 
#' Basic usage is to call either \code{\link{cv.svd.gabriel}} or
#' \code{\link{cv.svd.wold}}.
#' 
#' @useDynLib bcv, .registration=TRUE
#' @name bcv-package
#' @importFrom graphics lines points segments
#' @importFrom stats sd
#' @aliases bcv-package bcv
#' @docType package
#' @author Patrick O. Perry <patperry@@gmail.com>
#' @seealso \code{\link{impute.svd}}, \code{\link{cv.svd.gabriel}},
#' \code{\link{cv.svd.wold}}, \code{\link{plot.cvsvd}},
#' \code{\link{print.cvsvd}} \code{\link{summary.cvsvd}}
#' @keywords package
NULL
