#' Run the McCracy test for manipulation of the forcing variable
#' 
#' Calls the \code{\link{DCdensity}} test from package rdd on a \code{RDDobject}
#' 
#' @param RDDobject object of class RDDdata
#' @param bin Argument of the \code{\link{DCdensity}} function, the binwidth
#' @param bw Argument of the \code{\link{DCdensity}} function, the bandwidth
#' @export
#' @import rdd
#' @examples
#' library(RDDtools)
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' dens_test(Lee2008_rdd)



dens_test <- function(RDDobject, bin=NULL, bw=NULL){
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)
  test <- try(DCdensity(RDDobject$x, cutpoint, bin = bin, bw = bw), silent=TRUE)
  if(inherits(test, "try-error")){
    warning("Error in computing the density, returning a simple histogram", if(is.null(bin)) " with arbitrary bin" else NULL)
    if(is.null(bin)) {
      test <- try(DCdensity(RDDobject$x, cutpoint, bin = bin, bw = 0.2, ext.out=TRUE, plot=FALSE), silent=TRUE)
      bin <- test$binsize
    }
    max_x <- max(RDDobject$x, na.rm=TRUE)
    seq_breaks <- seq(from=min(RDDobject$x, na.rm=TRUE), to=max_x, by=bin)
    if(max_x>max(seq_breaks)) seq_breaks <- c(seq_breaks, max_x+0.001)
    hist(RDDobject$x, breaks=seq_breaks)
    abline(v=cutpoint, col=2, lty=2)
  }

  return(test)
}

if(FALSE){

library(RDDtools)
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

dens_test(Lee2008_rdd)

}