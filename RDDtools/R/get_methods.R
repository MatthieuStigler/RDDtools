

# checkIsRDD <- function(object)  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
# checkIsAnyRDD <- function(object)  if(!inherits(object, c("RDDdata", "RDDreg_np"))) stop("Only works for RDDdata objects")

# function(object)  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
checkIsAnyRDD <- checkIsRDD <- function(object)  {
  classesOk <- c("RDDdata", "RDDreg_np", "RDDreg_lm")
  if(!inherits(object, classesOk)) stop("Only works for RDDdata objects")
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



## return the type of inference used by RDDreg_np
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

getOriginalX.RDDdata <- function(object){
  object[, "x"]
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


#format(Sys.Date(), "%A %Y-%m-%d")

