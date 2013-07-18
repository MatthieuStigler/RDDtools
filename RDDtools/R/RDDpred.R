#' RDD coefficient prediction
#'
#' Function to predict the RDD coefficient in presence of covariate (without covariates, returns the same than \code{\link{RDDcoef}})
#' @param object A RDD regression object
#' @param covdata New data.frame specifying the values of the covariates, can have multiple rows. 
#' @param se.fit A switch indicating if standard errors are required.
#' @param vcov. Specific covariance function (see package sandwich ), by default uses the \code{\link{vcov}}
#' @param newdata Another data on which to evaluate the x/D variables. useful in very few cases. 
#' @details The function RDDpred does a simple prediction of the RDD effect
#'  \deqn{RDDeffect= \mu(x, z, D=1) - \mu(x, z, D=0)}
#' When there are no covariates (and z is irrelevant in the equation above), this amounts exactly to the usual RDD coefficient, 
#' shown in the outputs, or obtained with \code{\link{RDDcoef}}. If there were covariates, and if these covariates were added using the 
#' \dQuote{include} \emph{strategy}, and that furthermore they were estimated with different coefficients left and right to the cutoff (i.e.
#' had argument \emph{slope} = \dQuote{separate}), than the RDD effect is also dependent on the value of the covariate. 
#' RDDpred allows to set the value of the covariate(s) at which to evaluate the RDD effect, by providing a data.frame with
#' the values for the covariates. Note that the effect can be evaluated at multiple points, if you provide multiple rows of \code{covdata}. 
#' @return Returns the predicted value(s), and, if se.fit=TRUE, their standard errors. 
#' @export
#' @references Froehlich (2007) Regression discontinuity design with covariates, IZA discussion paper 3024
#' @examples
#' ## Load data, add (artificial) covariates:
#'   data(Lee2008)
#'   n_Lee <- nrow(Lee2008)
#'   z1 <- runif(n_Lee)
#'   Lee2008_rdd <- RDDdata(y=y, x=x, data=Lee2008, z=z1, cutpoint=0)
#' 
#' ## estimation without covariates: RDDpred is the same than RDDcoef:
#'   reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
#' 
#'   RDDpred(reg_para)
#'   RDDcoef(reg_para, allInfo=TRUE)
#' 
#' ## estimation with covariates: 
#'   reg_para_cov <- RDDreg_lm(RDDobject=Lee2008_rdd, covariates="z1", covar.opt=list(slope="separate"))
#'   RDDpred(reg_para_cov, covdata=data.frame(z1=0)) ## should obtain same result than with RDestimate
#'   RDDpred(reg_para_cov, covdata=data.frame(z1=0.5)) #evaluate at mean of z1 (as comes from uniform)

RDDpred <- function(object, covdata, se.fit=TRUE, vcov. = NULL, newdata, stat=c("identity", "mean")){

  stat <- match.arg(stat)

  x_call <- getCall(object)
  hasCo <- hasCovar(object)

  if(is.null(x_call$covar.opt)){
    covar.slope <- "same"
    covar.strat <- "include"
  } else {
    covar.slope <- ifelse(is.null(x_call$covar.opt$slope), "same", x_call$covar.opt$slope)
    covar.strat <- ifelse(is.null(x_call$covar.opt$strategy), "include", x_call$covar.opt$strategy)
  }


## get original data structure:
  mf <- model.frame(object)[1:2,-1]
  if(any(grepl("\\(weights\\)", colnames(mf)))) mf <- mf[,-grep("\\(weights\\)", colnames(mf))]

## Fill orig struc with 0/1
  if(missing(newdata)){
    which.D <- grep("^D$", colnames(mf))
    mf[,which.D] <- c(0,1) ## set coeff of interest
    mf[,-which.D] <- 0 ## remove others (not absolutely necessary actually)
    newdata <- mf
  }

## Merge covdata with newdata:

  if(!missing(covdata)){
    if(nrow(covdata)>1) newdata <- rbind(newdata[1,], Reduce(rbind, list(newdata[2,])[rep(1L, times=nrow(covdata))]))
    if(covar.strat=="residual") stop("Do not provide 'covdata' if covariates were use with 'residual' strategy")
    if(covar.slope=="separate"){
      ind <- seq(from=2, by=2, length.out=nrow(covdata))
      colnames_cov <- colnames(covdata)
      if(!all(colnames_cov%in% colnames(newdata))) stop("Arg 'covdata' contains colnames not in the data")
      newdata[2:nrow(newdata), paste(colnames(covdata), "D", sep=":")] <- covdata
    }
  } 

  multiN <- nrow(newdata)>2

## Set up variance matrix: X_i (X'X)^{-1} X_i'
  X_i <- as.matrix(cbind(1,newdata))
  if(any(is.na(X_i))){
    warning("data contains NA. Were removed")
    X_i <- X_i[-apply(X_i, 1, function(x) any(is.na(x))),]
  }
  if(is.null(vcov.)) vcov. <- vcov(object)
  X_inv <- vcov.
  mat <- X_i%*%X_inv%*%t(X_i)

## preds:

  pred_point <- drop(diff(X_i%*%coef(object)))
  if(se.fit) pred_se    <- sqrt(sum(c(diag(mat), -2*mat[1,2])))

  if(multiN) {
    d <- X_i%*%coef(object)

    Mat_DIFF <- cbind(-1, diag(nrow(d)-1))
    Mat_SUM  <- cbind( 1, diag(nrow(d)-1))
    Mat_DIAG <- matrix(diag(mat), ncol=1)
    MAT_SmallSum <- matrix(c(-(nrow(d)-1), rep(1,nrow(d)-1  )), nrow=1)

    if(stat=="identity"){
      pred_point <- drop(Mat_DIFF%*%d)
      if(se.fit) pred_se <- drop(sqrt(Mat_SUM %*%Mat_DIAG -2* mat[1,2:ncol(mat)]))
    } else {
      pred_point <- drop(MAT_SmallSum%*%d)
      if(se.fit) pred_se <- drop(sqrt(MAT_SmallSum%*%mat%*%t(MAT_SmallSum)))
    }
  }


## result:
  if(se.fit){
    res <- list()
    res$fit <- pred_point
    res$se.fit <- pred_se
  } else {
    res <- pred_point
  }
res
}

if(FALSE){
  library(RDDtools)
  data(Lee2008)
  head(Lee2008)

  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

  set.seed(123)
  n_Lee <- nrow(Lee2008)
  Z<- data.frame(z1=rnorm(n_Lee), z2=rnorm(n_Lee, mean=20, sd=2), z3=sample(letters[1:3], size=n_Lee, replace=TRUE))
  Lee2008_rdd_z <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z,cutpoint=0)

## use:
  reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)

  RDDpred(reg_para)
  RDDcoef(reg_para, allInfo=TRUE)
  all.equal(unlist(RDDpred(reg_para)), RDDcoef(reg_para, allInfo=TRUE)[1:2], check=FALSE)

## pred other coefs: 
  pred_Xr <- RDDpred(reg_para, newdata= data.frame(Tr=0, Xl=0, Xr=c(0,1)))
  all.equal(RDDcoef(reg_para, allInfo=TRUE, allCo=TRUE)[4,1:2], unlist(pred_Xr), check=FALSE)

  pred_Xl <- RDDpred(reg_para, newdata= data.frame(Tr=0, Xl=c(0,1), Xr=0))
  all.equal(RDDcoef(reg_para, allInfo=TRUE, allCo=TRUE)[3,1:2], unlist(pred_Xl), check=FALSE)

  reg_para2 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=2)
  RDDpred(reg_para2)
  all.equal(unlist(RDDpred(reg_para2)), RDDcoef(reg_para2, allInfo=TRUE)[1:2], check=FALSE)


### Covariates
  reg_para4_cov <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=1, covariates="z1", covar.opt=list(slope="separate"))
  reg_para4_cov
  summary(reg_para4_cov)

  RDDpred(reg_para4_cov)
  all.equal(unlist(RDDpred(reg_para4_cov)), RDDcoef(reg_para4_cov, allInfo=TRUE)[1:2], check=FALSE)

  all.equal(RDDpred(reg_para4_cov, covdata=data.frame(z1=0)),RDDpred(reg_para4_cov))

### Check RDDpred:
vec_eval <- c(2,4,4,5,6)
estim_sep <- lapply(vec_eval, function(x) RDDpred(object=reg_para4_cov, covdata=data.frame(z1=x)))
estim_toget <- RDDpred(reg_para4_cov, covdata=data.frame(z1=vec_eval))

all(estim_toget$fit==sapply(estim_sep, function(x) x$fit))
all(estim_toget$se.fit==sapply(estim_sep, function(x) x$se.fit))

environment(RDDpred) <- environment(RDDreg_lm)
sum(RDDpred(reg_para4_cov, covdata=data.frame(z1=c(0,1,2,1)))$fit) 
# RDDpred(x=reg_para4_cov, covdata=data.frame(z1=c(2,4,4,4,5,6)))
# RDDpred(reg_para4_cov)

}