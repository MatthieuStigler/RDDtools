#' Parametric polynomial estimator of the regression discontinuity
#' 
#' Compute a parametric polynomial regression of the ATE, 
#' possibly on the range specified by bandwidth
#' @param RDDobject Object of class RDDdata created by \code{\link{RDDdata}}
#' @param covariates TODO
#' @param bw A bandwidth to specify the subset on which the parametric regression is estimated
#' @param inference Type of inference to conduct: non-parametric one (\code{np}) or standard (\code{lm}). See details. 
#' @param slope Whether slopes should be different on left or right (separate), or the same.
#' @return An object of class RDDreg_np and class lm, with specific print and plot methods
#' @references TODO
#' @include plotBin.R
#' @export RDDreg_np
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


RDDreg_np <- function(RDDobject, covariates=NULL, bw=RDDbw_IK(RDDobject), slope=c("separate", "same"), inference=c("np", "lm")){

  slope <- match.arg(slope)
  inference <- match.arg(inference)
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)

  if(!is.null(covariates)) warning("covariates not fully implemented for non-para reg")
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

## Covariates

  if(!is.null(covariates)){
    covar <- getCovar(RDDobject)
    formu <- as.Formula(paste("y", covariates, sep="~"))
    mod_frame_cov <- model.frame(formu, covar, lhs=FALSE)
    dat_step1 <- cbind(dat_step1, mod_frame_cov) ## add covar as regressors
  } 


## Regression
  reg <- lm(y~., data=dat_step1, weights=kernel_w)
  coefD <- coef(reg)["D"]

## Non-para inference:
  if(inference=="np"){
    var <- var_estim(x=dat$x, y=dat$y, point=cutpoint, bw=bw, eachSide=TRUE)
    dens <- dens_estim(x=dat$x, point=cutpoint, bw=bw, eachSide=TRUE)

    const <- 4.8/(nrow(dat)*bw)
    all <- const*sum(var)/dens
    se <- sqrt(all)
    tval <- coefD/se
    pval <- 2 * pnorm(abs(tval), lower.tail = FALSE)
    coefmat <- matrix(c(coefD, se,tval, pval), nrow=1, dimnames=list("D", c("Estimate", "Std. Error", "z value", "Pr(>|z|)")))
  } else {
    coefmat <- coef(summary(reg))["D", , drop=FALSE]
  }

##Return
  res <- list()
  RDDslot <- list()
  RDDslot$RDDdata <- RDDobject
  RDDslot$model <- reg
  res$coefficients <- coef(reg)["D"]
  res$coefMat <- coefmat 
  res$residuals <- residuals(reg)
  res$fitted <- fitted(reg)
  res$RDDslot <- RDDslot

  class(res) <- c("RDDreg_np", "RDDreg", "lm")
  attr(res, "RDDcall") <- match.call()
  attr(res, "cutpoint") <- cutpoint
  attr(res, "bw") <- bw
  res
}


#' @S3method print RDDreg_np
print.RDDreg_np <- function(x, signif.stars = getOption("show.signif.stars"), ...) {

  RDDcall <- attr(x, "RDDcall")
  bw <- getBW(x)
  cutpoint <- getCutpoint(x)
  x_var <- getOriginalX(x)

  n_left  <- sum(x_var >= cutpoint -bw & x_var < cutpoint)
  n_right <- sum(x_var >= cutpoint & x_var <= cutpoint+bw)

  cat("### RDD regression: nonparametric local linear###\n")
  cat("\tBandwidth: ", bw, "\n")
  cat("\tNumber of obs: ", sum(n_left+n_right), " (left: ", n_left, ", right: ", n_right, ")\n", sep="")

  cat("\n\tCoefficient:\n")

  printCoefmat(x$coefMat, signif.stars=signif.stars)

}

#' @S3method summary RDDreg_np
summary.RDDreg_np <- function(object, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), ...) {

  x <- object
  bw <- getBW(x)
  cutpoint <- getCutpoint(x)
  x_var <- getOriginalX(x)

## compute numbers left/right:
  n_left  <- sum(x_var >= cutpoint -bw & x_var < cutpoint)
  n_right <- sum(x_var >= cutpoint & x_var <= cutpoint+bw)

## compute residual summary:
  res_quant <- quantile(residuals(x))
  names(res_quant) <- c("Min", "1Q", "Median", "3Q", "Max")

## compute R^2
  r.squared <- summary(x$RDDslot$model)$r.squared

## Extend the RDDreg_no output with new computaations:

  object$r.squared <- r.squared
  object$res_quant <- res_quant
  object$n_obs <- list(n_left=n_left, n_right=n_right, total=n_left+n_right)

  class(object) <- c("summary.RDDreg_np", class(object))
  object
}

#' @S3method print summary.RDDreg_np
print.summary.RDDreg_np <- function(x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), ...) {

  bw <- getBW(x)

  cat("### RDD regression: nonparametric local linear###\n")
  cat("\tBandwidth: ", bw, "\n")
  cat("\tNumber of obs: ", x$n_obs$total, " (left: ", x$n_obs$n_left, ", right: ", x$n_obs$n_right, ")\n", sep="")

  cat("\n\tWeighted Residuals:\n")
  print(zapsmall(x$res_quant, digits + 1))


  cat("\n\tCoefficient:\n")

  printCoefmat(x$coefMat, signif.stars=signif.stars)

  cat("\n\tLocal R squared:",  formatC(x$r.squared, digits = digits), "\n")

}


#' @S3method plot RDDreg_np
plot.RDDreg_np <- function(x,binwidth,chart=c("locpoly", "np"), ...) {

  chart <- match.arg(chart)
  cutpoint <- getCutpoint(x)
  bw <- getBW(x)
  if(missing(binwidth)) binwidth <- bw/5 # binwidth!=bandwidth

## data
  dat <- getOriginalData(x, classRDD=FALSE) 

## Use locpoly:
  dat_left <- subset(dat, x<cutpoint)
  dat_right <- subset(dat, x>=cutpoint)

  if(chart=="locpoly"){
    llp_left <- locpoly(x=dat_left$x, y=dat_left$y, bandwidth=bw)
    llp_right <- locpoly(x=dat_right$x, y=dat_right$y, bandwidth=bw)

## Use np:
  } else {
    np_reg_left  <- npreg(npregbw(y~x, data=dat_left, regtype="ll", ckertype="epanechnikov",
		      bandwidth.compute=FALSE, bws=bw))

     np_reg_right  <- npreg(npregbw(y~x, data=dat_right, regtype="ll", ckertype="epanechnikov",
		      bandwidth.compute=FALSE, bws=bw))
    newDat_left <- data.frame(x=seq(min(dat_left$x), cutpoint-0.001, by=.01))
    newDat_right <- data.frame(x=seq(cutpoint, max(dat_right$x),  by=.01))
    pred_left <- predict(np_reg_left, newdata=newDat_left,se.fit=TRUE)
    pred_right <- predict(np_reg_right, newdata=newDat_right,se.fit=TRUE)
  }
##plot
  plotBin(dat$x, dat$y, h=binwidth, ...)
  if(chart=="locpoly"){
    lines(llp_left$x, llp_left$y)
    lines(llp_right$x, llp_right$y)
  } else {
    lines(newDat_left$x, pred_left$fit, col=1)
    lines(newDat_left$x, pred_left$fit+2*pred_left$se.fit, col=2, lty=2)
    lines(newDat_left$x, pred_left$fit-2*pred_left$se.fit, col=2, lty=2)

    lines(newDat_right$x, pred_right$fit, col=1)
    lines(newDat_right$x, pred_right$fit+2*pred_right$se.fit, col=2, lty=2)
    lines(newDat_right$x, pred_right$fit-2*pred_right$se.fit, col=2, lty=2)
}
}



if(FALSE){
  library(RDDtools)
  data(Lee2008)
  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

  environment(RDDreg_np) <- environment(RDDdata)
  environment(plot.RDDreg_np) <- environment(RDDdata)
  environment(print.RDDreg_np) <- environment(RDDdata)
  environment(summary.RDDreg_np) <- environment(RDDdata)


  reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
  reg_nonpara_inflm <- RDDreg_np(RDDobject=Lee2008_rdd, inference="lm")
  RDDtools:::getCutpoint(reg_nonpara)
  head(RDDtools:::getOriginalX.RDDreg(reg_nonpara))

  print(reg_nonpara)
  print(reg_nonpara_inflm)
  summary(reg_nonpara)
  plot(x=reg_nonpara)
  plot(x=reg_nonpara, chart="np")
  plot(x=reg_nonpara, binwidth=0.05)


  RDDtools:::waldci.RDDreg_np(reg_nonpara)
  RDDtools:::waldci.RDDreg_np(reg_nonpara_inflm)

environment(waldci.RDDreg_np) <- environment(RDDdata)
waldci.RDDreg_np(reg_nonpara)

plotSensi(reg_nonpara)


class(getCall(reg_nonpara))
class(attr(reg_nonpara, "RDDcall"))


### MC
mc_simple <- function(n=10000, xr=0.1){
  x<- rnorm(n)
  y <- 1+1.2*x+ 1.4*ifelse(x>=0,1,0)+ xr*ifelse(x>=0,1,0)*x+rnorm(n)
  RD <- RDDdata(x=x, y=y, cutpoint=0)
  RD
}

r<-RDDreg_np(mc_simple())
summary(r)
plot(r)


}

if(FALSE){
bw <- RDDbw_IK(Lee2008_rdd)
dat <- Lee2008_rdd
x<- Lee2008_rdd$x
y<- Lee2008_rdd$y
cutpoint <- 0
  dat_left <- subset(dat, x<cutpoint)
  dat_right <- subset(dat, x>=cutpoint)

  llp_left <- locpoly(x=dat_left$x, y=dat_left$y, bandwidth=bw)
  llp_right <- locpoly(x=dat_right$x, y=dat_right$y, bandwidth=bw)

p1 <- -0.7346403
llp_left$x[which.min(abs(llp_left$x-p1))]
llp_left$y[which.min(abs(llp_left$x-p1))]

## around x: 
point <- -0.7350795

po <- subset(dat, x> point -bw & x< point+bw)
mean(po$y)
a<-  plotBin(dat$x, dat$y, h=bw)
a

a$x1 <- a$x-bw
a$x2 <- a$x+bw

b <- rownames(a)
b1 <- gsub("\\[|\\(","c(",b)
b2 <- gsub("\\]|\\)",")",b1)

mean(eval(parse(text=b2[1])))
diff(eval(parse(text=b2[1])))


  lines(llp_left$x, llp_left$y)
  lines(llp_right$x, llp_right$y)

}