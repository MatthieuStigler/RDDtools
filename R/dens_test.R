#' Run the McCracy test for manipulation of the forcing variable
#' 
#' Calls the \code{\link[rdd]{DCdensity}} test from package \code{rdd} on a \code{RDDobject}.
#' 
#' @param RDDobject object of class rdddata
#' @param bin Argument of the \code{\link{DCdensity}} function, the binwidth
#' @param bw Argument of the \code{\link{DCdensity}} function, the bandwidth
#' @param plot Whether to return a plot. Logical, default ot TRUE. 
#' @param \ldots Further arguments passed to \code{\link[rdd]{DCdensity}}. 
#' @export
#' @import rdd
#' @examples
#' data(Lee2008)
#' Lee2008_rdd <- rdddata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' dens_test(Lee2008_rdd)



dens_test <- function(RDDobject, bin=NULL, bw=NULL, plot=TRUE,...){
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)
  x <- getOriginalX(RDDobject)
  test <- try(DCdensity(runvar=x, cutpoint=cutpoint, bin = bin, bw = bw, plot=plot, ext.out=TRUE, ...), silent=TRUE)
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
  
  test.htest <- list()
  test.htest$statistic <- c("z-val"=test$z)
  test.htest$p.value <- test$p
  test.htest$data.name <- deparse(substitute(RDDobject))
  test.htest$method <- "McCrary Test for no discontinuity of density around cutpoint"
  test.htest$alternative <- "Density is discontinuous around cutpoint"
  test.htest$estimate <- c(Discontinuity=test$theta)
  test.htest$test.output <- test
  class(test.htest) <- "htest"
  return(test.htest)
}
