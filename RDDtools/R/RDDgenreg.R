#' General polynomial estimator of the regression discontinuity
#' 
#' Compute RDD estimate allowing a locally kernel weighted version of any estimation function
#' possibly on the range specified by bandwidth
#' @param RDDobject Object of class RDDdata created by \code{\link{RDDdata}}
#' @param covariates Formula to include covariates
#' @param order Order of the polynomial regression. 
#' @param bw A bandwidth to specify the subset on which the kernel weighted regression is estimated
#' @param covar.strat Way to include covariates, either in the main regression (\code{include}) or as regressors of y in a first step (\code{residual}). 
#' @param weights Optional weights to pass to the lm function. Note this cannot be entered together with \code{bw}
#' @param slope Whether slopes should be different on left or right (separate), or the same.
#' @param fun The function to estimate the parameters
#' @param \ldots Further argumetns passed to fun. See the example. 
#' @details This function allows the user to use a custom estimating function, instead of the traditional \code{lm()}. 
#' It is assumed that the custom funciton has following behaviour:
#' \enumerate{
#'   \item A formula interface, together with a \code{data} argument
#'   \item A \code{weight} argument
#'   \item A coef(summary(x)) returning a data-frame containing a column Estimate
#' }
#' Note that for the last requirement, this can be accomodated by writing a specific \code{\link{RDDcoef}} 
#' function for the class of the object returned by \code{fun}. 
#' @return An object of class RDDreg_lm and class lm, with specific print and plot methods
#' @references TODO
#' @include plotBin.R
#' @export RDDgenreg 
#' @examples
#' ## Step 0: prepare data
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' 
#' ## Estimate a local probit:
#' Lee2008_rdd$y <- with(Lee2008_rdd, ifelse(y<quantile(y, 0.25), 0,1))
#' reg_bin_glm <- RDDgenreg(RDDobject=Lee2008_rdd, fun= glm, family=binomial(link="probit"))
#' print(reg_bin_glm)
#' summary(reg_bin_glm)
#'

RDDgenreg <- function(RDDobject, covariates=NULL, order=1, bw=NULL, slope=c("separate", "same"), covar.strat=c("include", "residual"), weights, fun=glm, ...){
  
  slope <- match.arg(slope)
  covar.strat <- match.arg(covar.strat)
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)
  if(!missing(weights)&!is.null(bw)) stop("Cannot give both 'bw' and 'weights'")
  if(!is.null(covariates) & !hasCovar(RDDobject))  stop("Arg 'covariates' was specified, but no covariates found in 'RDDobject'.")
  
  ## Construct data
  dat <- as.data.frame(RDDobject)
  
  dat_step1 <- dat[, c("y", "x")]
  dat_step1$x <- dat_step1$x -cutpoint
  dat_step1$D <- ifelse(dat_step1$x>= 0, 1,0) ## NEW
  if(order>0){
    polys <- poly(dat_step1$x, degree=order, raw=TRUE)
    colnames(polys) <- paste("x", 1:order, sep="^")
    dat_step1  <- cbind(dat_step1[,c("y", "D")],polys)
    if(slope=="separate") {
      polys2 <- polys*dat_step1$D
      colnames(polys2) <- paste(colnames(polys), "right", sep="_")
      dat_step1  <- cbind(dat_step1,polys2)
    }
  } else {
    dat_step1$x <- NULL
  }
  
  
  ## Subsetting
  if(!is.null(bw)){
    weights <- ifelse(dat$x >= cutpoint -bw & dat$x <= cutpoint +bw, 1, 0)
  } else if(!missing(weights)){
    weights <- weights
  } else {
    weights <- NULL
  }
  
  ## Covariates
  if(!is.null(covariates)){
    covar <- getCovar(RDDobject)
    formu <- as.Formula(paste("y", covariates, sep="~"))
    mod_frame_cov <- model.frame(formu, covar, lhs=FALSE)
    
    if(covar.strat=="residual"){
      mod_frame_cov$y <- dat_step1$y
      first_stage <- fun(formu, data=mod_frame_cov) ## regress y on covariates only
      dat_step1$y <- residuals(first_stage) ## change in original data
    } else {
      dat_step1 <- cbind(dat_step1, mod_frame_cov) ## add covar as regressors
    }
  } 
  
  ## Regression
  reg <- fun(y~., data=dat_step1, weights=weights,...)
  
  ##Return
  RDDslot <- list()
  RDDslot$RDDdata <- RDDobject
  reg$RDDslot <- RDDslot 
  class(reg) <- c("RDDreg_lm", "RDDreg", class(reg))
  attr(reg, "PolyOrder") <- order
  attr(reg, "cutpoint") <- cutpoint
  attr(reg, "slope") <- slope
  attr(reg, "RDDcall") <- match.call()
  attr(reg, "bw") <- bw
  reg
}

RDDgenreg_old <- function(RDDobject, covariates=".", bw=RDDbw_IK(RDDobject), slope=c("separate", "same"), fun=glm, ...){

  slope <- match.arg(slope)
  checkIsRDD(RDDobject)
  if(!is.function(fun)) stop("Arg 'fun' should be a function")
  cutpoint <- getCutpoint(RDDobject)

## Construct data
  dat <- as.data.frame(RDDobject)

  dat_step1 <- dat[, c("y", "x")]
  dat_step1$x <- dat_step1$x -cutpoint
  dat_step1$D <- ifelse(dat_step1$x >= 0, 1,0)
  if(slope=="separate") {
    dat_step1$x_right <- dat_step1$x*dat_step1$D 
  }

### Weights
  kernel_w <- Kernel_tri(dat_step1[,"x"], center=0, bw=bw)

## Regression
  reg <- fun(y~., data=dat_step1, weights=kernel_w,...)

##Return
  class(reg) <- c("RDDreg_gen", "RDDreg", class(reg))
  attr(reg, "RDDcall") <- match.call()
  attr(reg, "cutpoint") <- cutpoint
  attr(reg, "bw") <- bw
  reg
}


if(FALSE){

  library(RDDtools)
  data(Lee2008)
  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

  reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
  environment(RDDgenreg) <- environment(RDDdata)
  reg_glm_norm <- RDDgenreg(RDDobject=Lee2008_rdd)

reg_nonpara
reg_glm_norm
plot(reg_glm_norm) 


### Binary example:

## gen from latent model:
gen_MC_binom <- function(n=200, LATE=0.3){
  x <- rnorm(n)
  D <- x>= 0
  y <- 0.8 + LATE*D+ 0.3*x+0.1*x*D+rnorm(n)
  y <- as.integer(ifelse(y> -0.5, 1, 0))
  if(mean(y==1)<0.04) y[sample(c(0,1), prob=c(0.1, 0.9), replace=TRUE, size=n)] <- 1
  RDDdata(x=x, y=y, cutpoint=0)
}

mc <- gen_MC_binom()
environment(RDDgenreg) <- environment(RDDdata)
reg_bin_glm <- RDDgenreg(RDDobject=mc, fun= glm, family=binomial(link="probit"))
  
## quantile:
  library(quantreg)
  MC1_dat <- gen_MC_IK()
  MC1_rdd <- RDDdata(y=MC1_dat$y, x=MC1_dat$x, cutpoint=0)
  
  RDDcoef.rq <- function(object, allInfo=FALSE, ...){
    res <- coef(summary(object))["D",, drop=FALSE]
    if(!allInfo) res <- res[,"coefficients"]
    res
  }
  
  reg_bin_rq1 <- RDDgenreg(RDDobject=MC1_rdd,  fun=rq, tau=0.5, bw=0.5)
  reg_bin_rq1
  coef(reg_bin_rq1)
  RDDcoef(reg_bin_rq1)
  RDDcoef(reg_bin_rq1, allInfo=TRUE)
  summary(reg_bin_rq1)
  
  pl_rq <- plotSensi(reg_bin_rq1, order=1, from=0.1, to=1)
  pl_rq
  

  
  
  
## Monte Carlo
  
doEs<- function(n){
mc <- gen_MC_binom()
  reg_bin_np <- RDDreg_np(RDDobject=mc)
  environment(RDDgenreg) <- environment(RDDdata)
  reg_bin_glm <- RDDgenreg(RDDobject=mc, fun= glm, family=binomial(link="probit"))
  reg_bin_glm_log <- RDDgenreg(RDDobject=mc, fun= glm, family=binomial(link="logit"))

a<- RDDtools:::RDDcoef(reg_bin_glm)/2.5
b<- RDDtools:::RDDcoef(reg_bin_glm_log)/4
d<- RDDtools:::RDDcoef(reg_bin_np)

res <- c(a, b, d)
names(res) <- c("Probit", "Logit", "LPM")
res
}

MC_logs <- replicate(500, doEs())

MC_logs2 <- t(MC_logs)
colMeans(MC_logs2)

colMeans(MC_logs2-0.2)
apply(MC_logs2, 2, sd)

colMeans(MC_logs2-0.2)^2+apply(MC_logs2, 2, var)
colMeans(MC_logs2-0.2)^2+apply(MC_logs2, 2, sd)

head(MC_logs)

reg_bin_glm
reg_bin_np

fav <- mean(dnorm(predict(reg_bin_glm, type = "link")))
fav * coef(swiss_probit)


}
