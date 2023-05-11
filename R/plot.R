#' Plot the Result of an SVD Cross-Validation
#' 
#' Plot the result of \code{\link{cv.svd.gabriel}} or
#' \code{\link{cv.svd.wold}}, optionally with error bars.
#' 
#' Plot the result of \code{\link{cv.svd.gabriel}} or
#' \code{\link{cv.svd.wold}}.  This plots a the estimated prediction error as a
#' function of rank, optionally with error bars.
#' 
#' If \code{add} is \code{TRUE}, the current plot is not cleared.
#' 
#' @param x the result of a \code{\link{cv.svd.gabriel}} or
#' \code{link{cv.svd.wold}} computation.
#' @param errorbars indicates whether or not to add error bars.
#' @param col the color to use for showing prediction error.
#' @param col.errorbars the color to use for the error bars.
#' @param add indicates whether or not to add to the current plot.
#' @param xlab the label for the x axis.
#' @param ylab the label for the y axis.
#' @param \dots additional arguments for \code{plot}.
#' @author Patrick O. Perry
#' @seealso \code{\link{cv.svd.gabriel}}, \code{\link{cv.svd.wold}},
#' \code{\link{print.cvsvd}} \code{\link{summary.cvsvd}}
#' @export
#' @examples
#' 
#'   # generate a rank-2 matrix plus noise
#'   n <- 50; p <- 20; k <- 2
#'   u <- matrix( rnorm( n*k ), n, k )
#'   v <- matrix( rnorm( p*k ), p, k )
#'   e <- matrix( rnorm( n*p ), n, p )
#'   x <- u %*% t(v) + e
#'   
#'   # perform 5-fold Wold-style cross-validtion
#'   cvw <- cv.svd.wold( x, 5, maxrank=10 )
#'   
#'   # perform (2,2)-fold Gabriel-style cross-validation
#'   cvg <- cv.svd.gabriel( x, 2, 2, maxrank=10 )
#'   
#'   # plot the results
#'   par( mfrow=c(2,1) )
#'   plot( cvw, main="Wold-style CV")
#'   plot( cvg, main="Gabriel-style CV")
#' 
plot.cvsvd <- function( x, errorbars = TRUE, add = FALSE,
                        xlab = "Rank", ylab = "Mean Sq. Prediction Error", 
                        col = "blue", col.errorbars = "gray50", 
                        ... ) {
    msep   <- x$msep
    maxrank <- x$maxrank
    
    K          <- nrow( msep )
    rank       <- seq( from=0, to=maxrank, by=1 )
    msep.mean  <- apply( msep, 2, mean )
    msep.se    <- apply( msep, 2, sd ) / sqrt( K )
    
    if( !add ) {
        if( errorbars ) {
            plot( c(rank-0.2,rank+0.2), msep.mean+c(-msep.se, msep.se), 
                  t='n', xlab=xlab, ylab=ylab, ... )
        } else {
            plot( rank, msep.mean, t='n', xlab=xlab, ylab=ylab, ... )
        }
    }

    lines( rank, msep.mean, col=col, ... )
    
    if( errorbars ) {
        segments( rank-0.2, msep.mean-msep.se, 
                  rank+0.2, msep.mean-msep.se,
                  col=col.errorbars )

        segments( rank, msep.mean-msep.se, 
                  rank, msep.mean+msep.se,
                  col=col.errorbars )

        segments( rank-0.2, msep.mean+msep.se, 
                  rank+0.2, msep.mean+msep.se,
                  col=col.errorbars )
    }

    points( rank, msep.mean, col=col, pch=16, cex=0.6 )
    
    invisible( list( k=rank, msep=msep.mean ) )
}
