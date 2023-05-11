#' Summarize the Result of an SVD Cross-Validation
#' 
#' Summarize the result of \code{\link{cv.svd.gabriel}} or
#' \code{\link{cv.svd.wold}}.
#' 
#' Print a table of the estimated prediction errors and the standard errors of
#' the estimate.  Put an asterisk (\code{*}) next to the minimum and a plus
#' (\code{+}) next to the "one standard error rule" choice.
#' 
#' @param object the result of a \code{\link{cv.svd.gabriel}} or
#' \code{\link{cv.svd.wold}} computation.
#' @param \dots additional arguments to \code{summary}.
#' @return \item{nfolds }{the number of cross-validation folds} \item{maxrank
#' }{the maximum rank for which prediction error is estimated.} \item{msep.mean
#' }{the average mean square error of prediction (MSEP) across all folds for
#' ranks 0, 1, ..., \code{maxrank}.} \item{msep.se }{the standard errors of the
#' \code{msep.mean} estimates.} \item{rank.best }{the rank with the minimum
#' \code{msep.mean} value.} \item{rank.1se }{the smallest rank within one
#' standard error of the minimum \code{msep.mean} value.}
#' @author Patrick O. Perry
#' @seealso \code{\link{cv.svd.gabriel}}, \code{\link{cv.svd.wold}},
#' \code{\link{plot.cvsvd}} \code{\link{print.cvsvd}}
#' @export
summary.cvsvd <- function( object, ... ) {
    msep    <- object$msep
    maxrank <- object$maxrank
    
    K         <- nrow( msep )
    rank      <- seq( from=0, to=maxrank, by=1 )
    msep.mean <- apply( msep, 2, mean )
    msep.se   <- apply( msep, 2, sd ) / sqrt( K )
    rank.best <- which.min( msep.mean ) - 1
    rank.1se  <- min( which( msep.mean 
                               <= msep.mean[ rank.best+1 ] 
                                  + msep.se[ rank.best+1 ] ) ) - 1
    
    names( rank.best ) <- NULL
                                  
    list( nfolds=K, maxrank=maxrank, 
          msep.mean=msep.mean, msep.se=msep.se, 
          rank.best=rank.best, rank.1se=rank.1se ) 
}
