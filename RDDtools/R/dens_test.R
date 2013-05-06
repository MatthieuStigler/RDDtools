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
  DCdensity(RDDobject$x, cutpoint, bin = bin, bw = bw)

}


if(FALSE){

library(RDDtools)
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

dens_test(Lee2008_rdd)

}