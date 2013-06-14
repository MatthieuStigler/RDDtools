#' Imbens-Kalyanaraman Optimal Bandwidth Calculation
#' 
#' Imbens-Kalyanaraman optimal bandwidth
#' for local linear regression in Regression discontinuity designs.
#' 
#' @param RDDobject of class RDDdata created by \code{\link{RDDdata}}
#' @param kernel The type of kernel used: either \code{triangular} or \code{uniform}. 
#' @return The optimal bandwidth
#' @references Imbens, Guido and Karthik Kalyanaraman. (2012) "Optimal Bandwidth Choice for the regression discontinuity estimator," 
#' Review of Economic Studies (2012) 79, 933-959
#' @export
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>
#' @examples
#' data(Lee2008)
#' rd<- RDDdata(x=Lee2008$x, y=Lee2008$y, cutpoint=0)
#' RDDbw_IK(rd)


RDDbw_IK <-function(RDDobject, kernel=c("Triangular", "Uniform", "Normal")) {

  kernel <- match.arg(kernel)
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)

  res <- RDDbw_IK_low(X=RDDobject$x,Y=RDDobject$y,threshold=cutpoint,verbose=FALSE, type="RES", returnBig=FALSE, kernel=kernel)
  return(res)

}

IK_bias <-function(RDDobject, kernel=c("Triangular", "Uniform", "Normal"), bw) {

  kernel <- match.arg(kernel)
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)

  resB <- RDDbw_IK_low(X=RDDobject$x,Y=RDDobject$y,threshold=cutpoint,verbose=FALSE, type="RES", returnBig=TRUE, kernel=kernel)

## compute C1: see IK equ 5, and Fan Jijbels (1996, 3.23)
# is done in R with locpol, computeMu(i=2, equivKernel(TrianK, nu=0, deg=1, lower=0, upper=1), lower=0, upper=1)
  C1 <- switch(kernel, "Triangular"= -0.1, "Uniform"= -0.1666667) ## from: 

## Compute bias as in IK equ:5, 
# note here 1/4 is outside C1
  if(missing(bw))  bw <- resB$h_opt
  res<-  C1 * 1/2 * bw^2 *(resB$m2_right-resB$m2_left)
  return(res)

}

IK_var <-function(RDDobject, kernel=c("Triangular", "Uniform"), bw) {

  kernel <- match.arg(kernel)
  checkIsRDD(RDDobject)
  cutpoint <- getCutpoint(RDDobject)

  resB <- RDDbw_IK_low(X=RDDobject$x,Y=RDDobject$y,threshold=cutpoint,verbose=FALSE, type="RES", returnBig=TRUE, kernel=kernel)

## compute C2: see IK equ 5, and Fan Jijbels (1996, 3.23)
# is done in R with locpol, computeRK(equivKernel(TrianK, nu=0, deg=1, lower=0, upper=1), lower=0, upper=1)
  C2 <- switch(kernel, "Triangular"= 4.8, "Uniform"= 4) ## from: 

## Compute var as in IK equ:5, 
  if(missing(bw))  bw <- resB$h_op
  elem1 <- (resB$var_inh_left+resB$var_inh_right)/resB$f_cu
  elem2 <- C2/(nrow(RDDobject)*bw)
  res <- elem1*elem2
  res
}

IK_amse <- function(RDDobject, kernel=c("Triangular", "Uniform"), bw) {

  var <- IK_var(RDDobject=RDDobject, kernel=kernel, bw=bw)
  bias <- IK_bias(RDDobject=RDDobject, kernel=kernel, bw=bw)
  res <- bias^2+var
  res
}


RDDbw_IK_low <-function (X,Y,threshold=0,verbose=FALSE, type=c("RES", "RES_imp","WP"), returnBig=FALSE, kernel=c("Triangular", "Uniform")) {
  
  type <- match.arg(type)
  kernel <- match.arg(kernel)


  N <- length(X)
  N_left  <- sum(X<threshold, na.rm=TRUE)
  N_right <- sum(X>=threshold, na.rm=TRUE)

  
##########
### STEP 1
##########

## Silverman bandwidth
  h1 <-  1.84*sd(X)*N^(-1/5)
  if(verbose)   cat("\n-h1:", h1)

## f(cut)
  isIn_h1_left  <- X>=(threshold-h1) & X<threshold
  isIn_h1_right <- X>=threshold & X<=(threshold+h1)

  NisIn_h1_left  <- sum(isIn_h1_left, na.rm=TRUE)
  NisIn_h1_right <- sum(isIn_h1_right, na.rm=TRUE)
  if(verbose)   cat("\n-N left /right:", NisIn_h1_left, NisIn_h1_right)


  f_cut <-(NisIn_h1_left+NisIn_h1_right)/(2*N*h1)
  if(verbose)   cat("\n-f(threshold):", f_cut)

## Variances : Equ (13) 

  var_inh_left  <- var(Y[isIn_h1_left], na.rm=TRUE)
  var_inh_right <- var(Y[isIn_h1_right], na.rm=TRUE)

# problem with working pap0er: Equ 4.9 is different!
  if(type=="WP"){
    denom <- 1/(NisIn_h1_left+NisIn_h1_right) 
    var_inh_global <- denom* ((NisIn_h1_left-1)* var_inh_left + (NisIn_h1_right-1)* var_inh_right)
  }

  if(verbose){
    cat("\n-Sigma^2 left:", var_inh_left, "\n-Sigma^2 right:", var_inh_right)
  }
##########
### STEP 2
##########


## Global function of order 3: Equ (14) 
  reg <-lm(Y~I(X>=threshold)+I(X-threshold)+I((X-threshold)^2)+I((X-threshold)^3))
  m3<- 6*coef(reg)[5]
  if(verbose)   cat("\n-m3:", m3)


## left and right bandwidths: Equ (15) 
  Ck_h2 <- 3.556702 # 7200^(1/7)
  h2_left  <- Ck_h2 * ( var_inh_left  /(f_cut*m3^2))^(1/7) * N_left^(-1/7)
  h2_right <- Ck_h2 * ( var_inh_right /(f_cut*m3^2))^(1/7) * N_right^(-1/7)

  if(verbose)   cat("\n-h2 left:", h2_left, "\n-h2 right:", h2_right)

## second derivatives right/left
  isIn_h2_left  <- X>=(threshold-h2_left) & X<threshold
  isIn_h2_right <- X>=threshold & X<=(threshold+h2_right)

  N_h2_left <- sum(isIn_h2_left, na.rm=TRUE)
  N_h2_right <- sum(isIn_h2_right, na.rm=TRUE)

  reg2_left  <-lm(Y~ I(X-threshold)+I((X-threshold)^2),subset=isIn_h2_left)
  reg2_right <-lm(Y~ I(X-threshold)+I((X-threshold)^2),subset=isIn_h2_right)

  m2_left  <- as.numeric(2*coef(reg2_left)[3])
  m2_right <- as.numeric(2*coef(reg2_right)[3])

  if(verbose)   cat("\n-m2 left:", m2_left, "\n-m2 right:", m2_right)

##########
### STEP 3
##########

## Regularization: Equ (16) 
  if(type=="RES"){
    r_left  <- (2160*var_inh_left)  / (N_h2_left *h2_left^4)
    r_right <- (2160*var_inh_right) / (N_h2_right*h2_right^4)
  } else {
    r_left  <- (2160*var_inh_global) / (N_h2_left *h2_left^4)
    r_right <- (2160*var_inh_global) / (N_h2_right*h2_right^4)
  }


  if(verbose)   cat("\n-Reg left:", r_left, "\n-Reg right:", r_right)

## Compute kernel dependent constant: (see file ~/Dropbox/HEI/rdd/Rcode/IK bandwidth/bandwidth_comput.R)
  Ck <- switch(kernel, "Triangular"=3.4375, "Uniform"=2.70192, "Normal"=1.25864) # is not 5.4 as in paper since our kernel is on I(|x|<1), not <1/2

## Final bandwidth: Equ (17) 
  h_opt <- Ck * ( (var_inh_left+ var_inh_right) / (f_cut * ((m2_right-m2_left)^2 + r_left +r_right)))^(1/5) * N^(-1/5)
  names(h_opt) <- "h_opt"

  if(verbose)   cat("\n\n")

###
  if(returnBig){
    res<- list()
    res$h_opt <- as.numeric(h_opt)
    res$var_inh_left <- var_inh_left
    res$var_inh_right <- var_inh_right
    res$m2_right <- m2_right
    res$m2_left <- m2_left
    res$f_cut <- f_cut
    res$h2_left <- h2_left
    res$h2_right <- h2_right
  } else {
  res <- h_opt
  }

 return(res)
}

if(FALSE){
  lee_dat4 <- read.csv("/home/mat/Dropbox/HEI/rdd/Rcode/IK bandwidth/datasets/imbens_from_MATLAB.csv", header=FALSE)
  colnames(lee_dat4) <- c("X", "Y")
  IKbandwidth3(X=lee_dat4$X, Y=lee_dat4$Y, verbose=TRUE)
  IKbandwidth3(X=lee_dat4$X, Y=lee_dat4$Y, verbose=TRUE, type="WP")
  IKbandwidth3(X=lee_dat4$X, Y=lee_dat4$Y, verbose=FALSE, returnBig=TRUE)


data(Lee2008)
Lee2008_rdd <- RDDdata(x=Lee2008$x,y=Lee2008$y , cutpoint=0)

### 
bw_IK <- RDDbw_IK(Lee2008_rdd)
bws <- sort(c(bw_IK, seq(0.05, 0.5, by=0.05)))
bi <- Vectorize(IK_bias, vectorize.args="bw")(Lee2008_rdd, bw=bws)
va <- Vectorize(IK_var, vectorize.args="bw")(Lee2008_rdd, bw=bws)
ms <- Vectorize(IK_amse, vectorize.args="bw")(Lee2008_rdd, bw=bws)

df<- data.frame(bw=rep(bws,3), value=c(ms, va, bi^2), type=rep(c("ms", "va", "bias^2"), each=length(bws)))


# qplot(x=bw, y=value, data=df, geom="line", colour=type)+geom_point(data=subset(df, value==min(subset(df, type=="ms", "value"))))

bws_03 <- sort(c(bw_IK, seq(0.25, 0.35, by=0.005)))
ms_03 <- Vectorize(IK_amse, vectorize.args="bw")(Lee2008_rdd, bw=bws_03)
df2 <- data.frame(bw=bws_03,mse=ms_03)

subset(df2, mse==min(mse)) ## 1.78, not 1.74 from: 
qplot(x=bw, y=mse, data=df2, geom="line")
}
