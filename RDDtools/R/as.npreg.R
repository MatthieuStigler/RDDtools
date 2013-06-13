
as.npregbw <- function(x,...){
  res <- as.npregbw_low(x=x, npreg=FALSE,...)
  res
}

as.npreg <- function(x,...){
  res <- as.npregbw_low(x=x, npreg=TRUE,...)
  res
}


as.npregbw_low <- function(x, npreg=FALSE,...){

  dat <- getOriginalData(x)
  bw <- getBW(x)
  cutpoint <- getCutpoint(x)

## Specify inputs to npregbw:
  x <- dat$x
  dat_np <- data.frame(y=dat$y, x=x,  D=ifelse(x>=0,1,0), Dx=ifelse(x>=0,x,0))
  range.x <- range(dat$x, na.rm=TRUE)
  bws <- c(bw, rep(9999*diff(range.x),2))
  dataPoints <- data.frame(x=c(cutpoint,cutpoint), D=c(0,1), Dx=c(0,cutpoint))

## start npregbw
  res <- npregbw(bws=bws, formula=y~x+D+Dx, data= dat_np,  regtype = "ll",
			eval=dataPoints, bandwidth.compute=FALSE, gradients=TRUE,...)

## if npreg, return instead model_np <- npreg(bw_np, newdata=dataPoints, gradients=TRUE)
  if(npreg) {
    res <- npreg(res, newdata=dataPoints, gradients=TRUE,...)
  }
  class(res) <- c("RDDreg_npreg", class(res))
  
  res
}


RDDcoef.RDDreg_npreg <- function(object, allInfo=FALSE, ...){

  co <- diff(object$mean)
  if(allInfo) {
    se <- sum(object$merr)
    zval <- co/se
    pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
    res <- cbind(co, se, zval, pval)
    colnames(res) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    rownames(res) <- "D"
  } else {
    res <- co
  }
  res 
}


if(FALSE){
  library(RDDtools)
  data(Lee2008)
  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
  reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)

  environment(as.npregbw_low) <- environment(RDDdata)
  reg_nonpara_npbw <- as.npregbw(reg_nonpara)
  reg_nonpara_npbw
class(reg_nonpara_npbw)
  reg_nonpara_np <- as.npreg(reg_nonpara)
  reg_nonpara_np
class(reg_nonpara_np)
RDDcoef(reg_nonpara_np)
RDDcoef(reg_nonpara_np, allInfo=TRUE)

## compare:
  bw_lm <- dnorm(Lee2008_rdd$x, sd=RDDtools:::getBW(reg_nonpara))
  reg_nonpara_gaus <- RDDreg_lm(RDDobject=Lee2008_rdd, w=bw_lm)
  all.equal(RDDcoef(reg_nonpara_gaus),RDDcoef(reg_nonpara_np))


}
