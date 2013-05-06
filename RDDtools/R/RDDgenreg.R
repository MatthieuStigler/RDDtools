#' Parametric polynomial estimator of the regression discontinuity
#' 
#' Compute a parametric polynomial regression of the ATE, 
#' possibly on the range specified by bandwidth
#' @param RDDobject Object of class RDDdata created by \code{\link{RDDdata}}
#' @param covariates TODO
#' @param bw A bandwidth to specify the subset on which the parametric regression is estimated
#' @param slope Whether slopes should be different on left or right (separate), or the same.
#' @param fun Function estimating the model
#' @param \ldots Furtehr arguments passed to fun
#' @return An object of class RDDreg_np and class lm, with specific print and plot methods
#' @references TODO
#' @include plotBin.R
##  @export RDDgenreg ####export after!
#' @examples
#' ## Step 0: prepare data
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' ## Step 2: regression
#' # Simple polynomial of order 1:
#' reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
#' print(reg_nonpara)
#' plot(reg_nonpara)
#'


RDDgenreg <- function(RDDobject, covariates=".", bw=RDDbw_IK(RDDobject), slope=c("separate", "same"), fun=glm, ...){

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
  class(reg) <- c("RDDreg_np", "lm")
  attr(reg, "RDDcall") <- match.call()
  attr(reg, "cutpoint") <- cutpoint
  attr(reg, "bw") <- bw
  reg
}


if(FALSE){

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


doEs<- function(n){
mc <- gen_MC_binom()
  reg_bin_np <- RDDreg_np(RDDobject=mc)
  environment(RDDgenreg) <- environment(RDDdata)
  reg_bin_glm <- RDDgenreg(RDDobject=mc, fun= glm, family=binomial(link="probit"))
  reg_bin_glm_log <- RDDgenreg(RDDobject=mc, fun= glm, family=binomial(link="logit"))

a<- RDDtools:::getEst(reg_bin_glm)/2.5
b<- RDDtools:::getEst(reg_bin_glm_log)/4
d<- RDDtools:::getEst(reg_bin_np)

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
