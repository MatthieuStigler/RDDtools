### MISC
is.even <- function (a) {
    a%%2 == 0
}

# checkIsRDD <- function(object)  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
# checkIsAnyRDD <- function(object)  if(!inherits(object, c("RDDdata", "RDDreg_np"))) stop("Only works for RDDdata objects")

# function(object)  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
checkIsAnyRDD <- checkIsRDD <- function(object)  {
  classesOk <- c("RDDdata", "RDDreg_np", "RDDreg_lm")
  if(!inherits(object, classesOk)) stop("Only works for RDDdata objects")
}


Kernel_tri <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1 - (abs(X - center) / bw))
}


getCutpoint <- function(object){

  checkIsRDD(object)
  attr(object, "cutpoint")
}

getOrder <- function(object){

  checkIsRDD(object)
  attr(object, "PolyOrder")
}

getSlope <- function(object){

  checkIsRDD(object)
  attr(object, "slope")
}

getBW <- function(object, force.na=FALSE){

  checkIsAnyRDD(object)
  res <- attr(object, "bw")
  if(force.na) if(is.null(res)) res <- NA
  res
}

#' RDD coefficient
#'
#' Function to access the RDD coefficient in the various regressions
#' @param object A RDD regression object
#' @param allInfo whether to return just the coefficients (allInfo=FALSE) or also the se/t stat/pval. 
#' @param allCo Whether to give only the RDD coefficient (allCo=FALSE) or all coefficients
#' @param \ldots Further arguments passed to/from specific methods
#' @return Either a numeric value of the RDD coefficient estimate, or a data frame with the estimate, 
#' its standard value, t test and p-value and 
#' @export
RDDcoef <- function(object, allInfo=FALSE, allCo=FALSE, ...)
  UseMethod("RDDcoef")

#' @rdname RDDcoef
#' @method RDDcoef default
#' @S3method RDDcoef default
RDDcoef.default <- function(object, allInfo=FALSE, allCo=FALSE, ...){
  res <- coef(summary(object))
  if(!allCo) res <- res["D",, drop=FALSE]
  if(!allInfo) res <- res[,"Estimate"]
  res
}

#' @rdname RDDcoef
#' @method RDDcoef RDDreg_np
#' @S3method RDDcoef RDDreg_np
RDDcoef.RDDreg_np <- function(object, allInfo=FALSE, allCo=FALSE, ...){
  res<- object$coefMat
  if(!allCo) res <- res["D",, drop=FALSE]
  if(!allInfo) res <- res[,"Estimate"]
  res
}

## return the typoe of inference used by RDDreg_np
infType <- function(x) {
  if(is.null(getCall(x)$inference)) "se" else getCall(x)$inference
}


hasCovar <- function(object)
  UseMethod("hasCovar")

hasCovar.RDDdata <- function(object)  attr(object, "hasCovar")

hasCovar.RDDreg <- function(object) { 
  call <- getCall(object)
  !is.null(call$covariates)
}

getCovar <- function(object){
  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
  if(!hasCovar(object)) stop("object has no covariates")

  res <- object[,-c(1,2), drop=FALSE]
  as.data.frame(res)
}



getOriginalX <- function(object){

  cutpoint <- getCutpoint(object)
  x <- object$model[,"x"]
  if(cutpoint!=0)  x <- x+cutpoint
  x
}

getOriginalX <- function(object)
  UseMethod("getOriginalX")


getOriginalX.RDDreg <- function(object){
  object$RDDslot$RDDdata[, "x"]
}

# getOriginalX.RDDreg_np <- function(object){
# 
#   cutpoint <- getCutpoint(object)
#   Xnam <- getXname(object) 
#   x <- object$model[,Xnam]
#   if(cutpoint!=0)  x <- x+cutpoint
#   x
# }


getOriginalData <- function(object, na.rm=TRUE, classRDD=TRUE)
  UseMethod("getOriginalData")

# getOriginalData.RDDreg_np <- function(object, na.rm=TRUE){
# 
#   cutpoint <- getCutpoint(object)
#   Xnam <- getXname(object) 
#   dat <- object$model[,c("y",Xnam)]
#   if(cutpoint!=0)  dat[,Xnam] <- dat[,Xnam] +cutpoint
#   if(na.rm) dat <- dat[apply(dat, 1, function(x) all(!is.na(x))),] # remove na rows
#   dat
# }



getOriginalData.RDDreg <- function(object, na.rm=TRUE, classRDD=TRUE){
  res <- object$RDDslot$RDDdata
  if(na.rm) res <- res[apply(res, 1, function(x) all(!is.na(x))),] # remove na rows
  if(!classRDD) res <- as.data.frame(res)
  res
}



#' @importFrom stats getCall
#' @S3method getCall RDDreg
getCall.RDDreg <- function(x,...) attr(x, "RDDcall")

.onLoad <- function(libname, pkgname)
    packageStartupMessage("\nRDDtools ", utils:::packageVersion("RDDtools"), 
      "\nPLEASE NOTE THIS is currently only a development version. \nRun vignette('RDDtools') for the documentation")

#format(Sys.Date(), "%A %Y-%m-%d")

