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

getBW <- function(object){

  checkIsAnyRDD(object)
  attr(object, "bw")
}

getEst <- function(object, all=FALSE){
  res <- coef(summary(object))["D",, drop=FALSE]
  if(!all) res <- res[,1]
  res
}




hasCovar <- function(object){
  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
  attr(object, "hasCovar")
}

getCovar <- function(object){
  if(!inherits(object, "RDDdata")) stop("Only works for RDDdata objects")
  if(!hasCovar(object)) stop("object has no covariates")

  res <- object[,-c(1,2)]
  res
}

getXname <- function(object){
  if(inherits(object, "RDDreg_lm")) "x^1" else "x"
}


getOriginalX <- function(object){

  cutpoint <- getCutpoint(object)
  Xnam <- getXname(object) 
  x <- object$model[,Xnam]
  if(cutpoint!=0)  x <- x+cutpoint
  x
}

getOriginalData <- function(object){

  cutpoint <- getCutpoint(object)
  Xnam <- getXname(object) 
  dat <- object$model[,c("y",Xnam)]
  if(cutpoint!=0)  dat[,Xnam] <- dat[,Xnam] +cutpoint
  dat
}


getCall.RDDreg_np <- function(x,...) attr(x, "RDDcall")

.onLoad <- function(libname, pkgname)
   packageStartupMessage("\nRDDtools ", utils:::packageVersion("RDDtools"), " (rev ", format(Sys.Date(), "%A"), " ", Sys.Date(),"). PLEASE NOTE THIS is currently only a development version. \nRun vignette('RDDtools') for the documentation")
