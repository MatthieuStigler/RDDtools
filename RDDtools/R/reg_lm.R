#' Parametric polynomial estimator of the regression discontinuity
#' 
#' Compute a parametric polynomial regression of the ATE, 
#' possibly on the range specified by bandwidth
#' @param RDDobject Object of class RDDdata created by \code{\link{RDDdata}}
#' @param covariates TODO
#' @param order Order of the polynomial regression. 
#' @param bw A bandwidth to specify the subset on which the parametric regression is estimated
#' @param slope Whether slopes should be different on left or right (separate), or the same.
#' @return An object of class RDDreg_lm and class lm, with specific print and plot methods
#' @references TODO
#' @include plotBin.R
#' @include Misc.R
#' @export
#' @examples
#' ## Step 0: prepare data
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' ## Step 2: regression
#' # Simple polynomial of order 1:
#' reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
#' print(reg_para)
#' plot(reg_para)
#'
#' # Simple polynomial of order 4:
#' reg_para4 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=4)
#' reg_para4
#' plot(reg_para4)
#'
#' # Restrict sample to bandwidth area:
#' bw_ik <- RDDbw_IK(Lee2008_rdd)
#' reg_para_ik <- RDDreg_lm(RDDobject=Lee2008_rdd, bw=bw_ik, order=4)
#' reg_para_ik
#' plot(reg_para_ik)


RDDreg_lm <- function(RDDobject, covariates=".", order=1, bw=NULL, slope=c("separate", "same")){

  slope <- match.arg(slope)
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)

## Construct data
  dat <- as.data.frame(RDDobject)

  dat_step1 <- dat[, c("y", "x")]
  dat_step1$x <- dat_step1$x -cutpoint
  dat_step1$D <- ifelse(dat_step1$x>= 0, 1,0) ## NEW
  polys <- poly(dat_step1$x, degree=order, raw=TRUE)
  colnames(polys) <- paste("x", 1:order, sep="^")
  dat_step1  <- cbind(dat_step1[,c("y", "D")],polys)
  if(slope=="separate") {
    polys2 <- polys*dat_step1$D
    colnames(polys2) <- paste(colnames(polys), "right", sep="_")
    dat_step1  <- cbind(dat_step1,polys2)
  }


## Subsetting
  if(!is.null(bw)){
    isIn <- dat$x >= cutpoint -bw & dat$x <= cutpoint +bw
  } else {
    isIn <- rep(TRUE, length(dat$x))
  }

## Regression
  reg <- lm(y~., data=dat_step1, subset=isIn)

##Return
  class(reg) <- c("RDDreg_lm", "RDDreg", "lm")
  attr(reg, "PolyOrder") <- order
  attr(reg, "cutpoint") <- cutpoint
  attr(reg, "slope") <- slope
  attr(reg, "RDDcall") <- match.call()
  attr(reg, "bw") <- bw
  reg
}


#' @S3method print RDDreg_lm
print.RDDreg_lm <- function(x,...) {

  order <- getOrder(x)
  cutpoint <- getCutpoint(x)
  slope <- getSlope(x)
  bw <- getBW(x)
  hasBw <- !is.null(bw)
  bw2 <- if(hasBw) bw else Inf

  x_var <- getOriginalX(x)
  n_left  <- sum(x_var >= cutpoint -bw2 & x_var < cutpoint)
  n_right <- sum(x_var >= cutpoint & x_var <= cutpoint+bw2)

  cat("### RDD regression: parametric ###\n")
  cat("\tPolynomial order: ", order, "\n")
  cat("\tSlopes: ", slope, "\n")
  if(hasBw)   cat("\tBandwidth: ", bw, "\n")
  cat("\tNumber of obs: ", sum(n_left+n_right), " (left: ", n_left, ", right: ", n_right, ")\n", sep="")

  cat("\n\tCoefficient:\n")

  printCoefmat(coef(summary(x))[2,, drop=FALSE])

}

#' @S3method plot RDDreg_lm
plot.RDDreg_lm <- function(x,...) {

## data
  dat <- getOriginalData(x)
  pred <- cbind(dat[,"x^1"],fitted(x))[order(dat[,"x^1"]),]
  
##plot
  plotBin(dat[,"x^1"], dat$y, ...)
  lines(pred)
}



if(FALSE){

  library(RDDtools)
  data(Lee2008)

  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)


  reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
  print(x=reg_para )
  summary(reg_para )

  reg_para_same <- RDDreg_lm(RDDobject=Lee2008_rdd, slope="same")
  print(x=reg_para_same )
  summary(reg_para_same )

  reg_para2 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=2)
  reg_para2
  summary(reg_para2)
  plot(reg_para2)

  reg_para2_same <- RDDreg_lm(RDDobject=Lee2008_rdd, order=2, slope="same")
  reg_para2_same
  summary(reg_para2_same)
  plot(reg_para2)

  bw_ik <- RDDbw_IK(Lee2008_rdd)
  reg_para_ik <- RDDreg_lm(RDDobject=Lee2008_rdd, bw=bw_ik)
  print(x=reg_para_ik)
  plot(x=reg_para_ik)

}