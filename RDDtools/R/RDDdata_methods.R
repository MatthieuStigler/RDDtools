

### SUMMARY method
#' @S3method summary RDDdata
summary.RDDdata <- function(object, ...){

  cutpoint <- getCutpoint(object)
  hasCovar_eng <- ifelse(hasCovar(object), "yes", "no")
  cat("### RDDdata object ###\n")
  cat("\nCutpoint:", cutpoint, "\n")
  cat("Sample size:",
	"\n\t-Full :", nrow(object), 
	"\n\t-Left :", sum(object$x<cutpoint), 
	"\n\t-Right:", sum(object$x>=cutpoint))
  cat("\nCovariates:", hasCovar_eng, "\n")
}

#' Plot RDDdata
#' 
#' Binned plot of the forcing and outcome variable
#' 
#' @param x Object of class RDDdata
#' @param h The binwidth parameter (note this differs from the bandwidth parameter!)
#' @param nbins Alternative to h, the total number of bins in the plot.
#' @param xlim The range of the x data
#' @param cex Size of the points, see \code{\link{par}}
#' @param nplot Number of plot to draw
#' @param device Type of device used. Currently not used.
#' @param \ldots Further arguments passed to the \code{\link{plot}} function.
#' @return A plot
#' @details Produces a simple binned plot averaging values within each interval. The length of the intervals
#' is specified with the argument \code{h}, specifying the whole binwidth (contrary to the usual bandwidth
#' argument, that gives half of the length of the kernel window. 
#' When no bandwidth is given, the bandwidth of Ruppert et al is used, see \code{\link{RDDbw_RSW}}. 
#' 
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>
#' @examples
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' plot(Lee2008_rdd)
#' 
#' ## Specify manually the bandwidth:
#' plot(Lee2008_rdd, h=0.2)
#' 
#' ## Show three plots with different bandwidth:
#' plot(Lee2008_rdd, h=c(0.2,0.3,0.4), nplot=3)
#' 
#' ## Specify instead of the bandwidth, the final number of bins:
#' plot(Lee2008_rdd, nbins=22)
#'
#' ## If the specified number of bins is odd, the larger number is given to side with largest range
#' plot(Lee2008_rdd, nbins=21)
#' @method plot RDDdata
#' @S3method plot RDDdata


### PLOT method
plot.RDDdata <- function(x, h, nbins=NULL, xlim=range(object$x, na.rm=TRUE), cex=0.7, nplot=1, device=c("base", "ggplot"),...){

  object <- x
  cutpoint <- getCutpoint(object)
  device <- match.arg(device)

## bandwidth: use Ruppert, Sheather and Wand (KernSmooth:::dpill)
  if(missing(h) & is.null(nbins)) {
    if(!all(xlim==range(object$x, na.rm=TRUE))){
      object <- subset(object, x> min(xlim) & x< max(xlim))
    }
    h <- RDDbw_RSW(object) 
    if(is.even(nplot)) {
      se <- seq(from=1-(sum(1:nplot<(nplot/2)))*0.2, to=1+(sum(1:nplot>(nplot/2)))*0.2, by=.2)
    } else {
      se <- seq(from=1-floor(nplot/2)*0.2, to=1+floor(nplot/2)*0.2, by=.2)
    }
    hs <- if(nplot==1) h else se *h
  } else if(!missing(h) & is.null(nbins)){
    if(length(h)==1){
      if(is.even(nplot)) {
	se <- seq(from=1-(sum(1:nplot<(nplot/2)))*0.2, to=1+(sum(1:nplot>(nplot/2)))*0.2, by=.2)
      } else {
	se <- seq(from=1-floor(nplot/2)*0.2, to=1+floor(nplot/2)*0.2, by=.2)
      }
      hs <- if(nplot==1) h else se *h
    } else {
      if(length(h==nplot)){
	hs <- h
      } else {
	stop("Length of h should be either one or equal to nplot (", nplot, ")")
      }
    }
  } else if(!is.null(nbins)){
    hs <- rep(0.05, nplot)
    if(length(nbins)!=nplot){
      stop("Length of nbins should be equal to nplot (", nplot, ")")
    }
  }


  

## plot

  par_orig <- par()
  par(mfrow=c(nplot,1))
  for(i in 1:nplot){
    plotBin(x=object$x, y=object$y, cutpoint=cutpoint, h=hs[i], nbins=nbins[i], xlim=xlim, cex=cex,...)
  }
  par(mfrow=c(1,1))



## invisible return:
  invisible(object)
}



#' Convert a rdd object to lm
#' @param x An object to convert to lm

#' @export
as.lm <- function(x)
  UseMethod("as.lm")


as.lm_RDD <- function(x){

  at_x <- attributes(x)
  at_x[names(at_x)!="names"] <- NULL
  class(x) <- "lm"

  x
}

#' @S3method as.lm RDDreg_np
as.lm.RDDreg_np <- function(x) as.lm_RDD(x)

#' @S3method as.lm RDDreg
as.lm.RDDreg <- function(x) as.lm_RDD(x)




# subset.RDDdata <- function(x,...){
# 
#   res <- subset.data.frame(x,...)
#   attributes(res) <- attributes(x)
#   res
# }


### EXAMPLE
if(FALSE){
  library(RDDtools)
#   data(Lee2008)


  environment(plot.RDDdata) <- environment(RDDdata)

  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
  plot(Lee2008_rdd)

  plot(Lee2008_rdd, h=0.2)
  plot(Lee2008_rdd, h=c(0.2,0.3,0.4), nplot=3)

  plot(Lee2008_rdd, nbins=21)

}
