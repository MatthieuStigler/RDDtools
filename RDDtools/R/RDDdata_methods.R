

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


#' @S3method plot RDDdata

### PLOT method
plot.RDDdata <- function(x,  h, xlim=range(object$x, na.rm=TRUE), cex=0.7, nplot=1,type=c("base", "ggplot"),...){

  object <- x
  cutpoint <- getCutpoint(object)
  type <- match.arg(type)

## bandwidth: use Ruppert, Sheather and Wand (KernSmooth:::dpill)
  if(missing(h)) {
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
  } else {
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
  }


  

## plot

  par_orig <- par()
  par(mfrow=c(nplot,1))
  for(i in 1:nplot){
    plotBin(x=object$x, y=object$y, cutpoint=cutpoint, h=hs[i], xlim=xlim, cex=cex,...)
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
#   lee_dat4 <- read.csv("/home/mat/Dropbox/HEI/rdd/Rcode/IK bandwidth/datasets/imbens_from_MATLAB.csv", header=FALSE)
#   head(lee_dat4)
# 
#   a<-RDDdata(y=lee_dat4[,2], x=lee_dat4[,1], cutpoint=0)
# head(a)
# getCutpoint(a)
# hasCovar(a)
# summary(a)
# plot(a)
# plot(a, nplot=1)
# plot(object=a, xlim=c(-0.5, 0.5))
# plot(a, xlim=c(-0.5, 0.5), h=c(0.1, 0.05, 0.01))
}
