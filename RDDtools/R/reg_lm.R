#' Parametric polynomial estimator of the regression discontinuity
#' 
#' Compute a parametric polynomial regression of the ATE, 
#' possibly on the range specified by bandwidth
#' @param RDDobject Object of class RDDdata created by \code{\link{RDDdata}}
#' @param covariates Formula to include covariates
#' @param order Order of the polynomial regression. 
#' @param bw A bandwidth to specify the subset on which the parametric regression is estimated
#' @param covar.strat DEPRECATED, use covar.opt instead. 
#' @param covar.opt Options for the inclusion of covariates. Way to include covariates, either in the main regression (\code{include}) or as regressors of y in a first step (\code{residual}). 
#' @param weights Optional weights to pass to the lm function. Note this cannot be entered together with \code{bw}
#' @param slope Whether slopes should be different on left or right (separate), or the same.
#' @return An object of class RDDreg_lm and class lm, with specific print and plot methods
#' @details This function estimates the standard \emph{discontinuity regression}:
#' \deqn{Y=\alpha+\tau D+\beta_{1}(X-c)+\beta_{2}D(X-c)+\epsilon}
#' with \eqn{\tau} the main parameter of interest. Several versions of the regression can be estimated, either restricting the slopes to be the same, 
#' i.e \eqn{\beta_{1}=\beta_{2}} (argument \code{slope}). The order of the polynomial in \eqn{X-c} can also be adjusted with argument \code{order}. 
#' Note that a value of zero can be used, which corresponds to the simple \emph{difference in means}, that one would use if the samples were random. 
#' Covariates can also be added in the regression, according to the two strategies discussed in Lee and Lemieux (2010, sec 4.5), through argument \code{covar.strat}:
#' \describe{
#' \item{include}{Covariates are simply added as supplementary regressors in the RD equation}
#' \item{residual}{The dependent variable is first regressed on the covariates only, then the RDD equation is applied on the residuals from this first step}}
#' The regression can also be estimated in a neighborhood of the cutpoint with the argument \code{bw}. This make the parametric regression resemble 
#' the non-parametric local kernel \code{\link{RDDreg_np}}. Similarly, weights can also be provided (but not simultaneously to \code{bw}). 
#'
#' The returned object is a classical \code{lm} object, augmented with a \code{RDDslot}, so usual methods can be applied. As is done in general in R, 
#' heteroskeadsticity-robust inference can be done later on with the usual function from package \pkg{sandwich}. For the case of clustered observations
#' a specific function \code{\link{clusterInf}} is provided.  
#' @references TODO
#' @include plotBin.R
#' @import Formula
#' @importFrom AER ivreg
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


RDDreg_lm <- function(RDDobject, covariates=NULL, order=1, bw=NULL, slope=c("separate", "same"), covar.opt=list(strategy=c("include", "residual"), slope=c("same", "separate"), bw=NULL), covar.strat=c("include", "residual"), weights){

  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)
  type <- getType(RDDobject)

  slope <- match.arg(slope)

  if(!missing(covar.strat)) warning("covar.strat is (soon) deprecated arg!")  
  if(!missing(weights)&!is.null(bw)) stop("Cannot give both 'bw' and 'weights'")

## Subsetting
  dat <- as.data.frame(RDDobject)

  if(!is.null(bw)){
    weights <- ifelse(dat$x >= cutpoint -bw & dat$x <= cutpoint +bw, 1, 0)
  } else if(!missing(weights)){
    weights <- weights
  } else {
    weights <- NULL
  }

## Construct data
  if(missing(weights)) weights <- NULL
  dat_step1 <- model.matrix(RDDobject, covariates=covariates, order=order, bw=bw, 
			    slope=slope, covar.opt=covar.opt)

## Regression
  if(type=="Sharp"){
    reg <- lm(y~., data=dat_step1, weights=weights)
    class_reg <- "lm"
  } else {
    if(!is.null(covariates)) stop("Covariates currently not implemented for Fuzzy case")
#     status <- RDDobject$z
dat_step1 <<- dat_step1
    print(head(dat_step1))
    reg <- ivreg(y~.-ins|.-D, data=dat_step1, weights=weights)
    class_reg <- "ivreg"
  }
  

##Return
  RDDslot <- list()
  RDDslot$RDDdata <- RDDobject
  reg$RDDslot <- RDDslot 
  class(reg) <- c("RDDreg_lm", "RDDreg", class_reg)
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
  subw <-   if(!is.null(x$weights)) x$weights>0 else rep(TRUE, nrow(dat))
  pred <- data.frame(x=dat$x,y=fitted(x))[subw,]
  
##plot
  plotBin(dat$x, dat$y, ...)
  lines(pred[order(pred$x),])
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