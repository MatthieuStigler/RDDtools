#' Post-inference for clustered data
#' 
#' Correct standard-errors to account for clustered data, doing either a degrees of freedom correction or using a heteroskedasticidty-cluster robust covariance matrix
#' possibly on the range specified by bandwidth
#' @param object Object of class lm, from which RDDreg also inherits.
#' @param clusterVar The variable containing the cluster attributions. 
#' @param vcov. Specific covariance function to pass to coeftest. See help of sandwich
#' @param type The type of cluster correctin to do: either the degrees of freedom, or a HC matrix. 
#' @param \ldots Further argumetns passed to coeftest
#' @return The output of the coeftest function, which is itself of class \code{coeftest}
#' @export
#' @import sandwich
#' @import lmtest
#' @examples
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
#' 
#' # here we just generate randomly a cluster variable:
#' nlet <- sort(c(outer(letters, letters, paste, sep="")))
#' clusRandom <- sample(nlet[1:60], size=nrow(Lee2008_rdd), replace=TRUE)
#'
#' # now do post-inference:
#' clusterInf(reg_para, clusterVar=clusRandom)
#' clusterInf(reg_para, clusterVar=clusRandom, type="HC")


clusterInf <- function(object, clusterVar, vcov. = NULL, type=c("df-adj", "HC"), ...){


  type <- match.arg(type)

  npar <- length(coef(object))
  nClus <- if(is.factor(clusterVar)) nlevels(clusterVar) else length(unique(clusterVar))

  if(type=="HC" & !is.null(vcov.)) warning("arg 'vcov.' not used when 'type=HC'")

  if(type=="df-adj"){
    res <- coeftest(object, vcov. = vcov., df = nClus, ...)
  } else {
    res <- coeftest(object, vcov. = function(x) vcovCluster(x, clusterVar=clusterVar), ...)
  }

  return(res)
}

#' @S3method estfun RDDreg_np
estfun.RDDreg_np <- function(x,...){
  inf_met <- infType(x) ## def in Misc.R
  if(inf_met=="se") stop("No 'vcovHC', 'vcovCluster', 'estfun' etc can be applied to RDDrg_np with non-parametric inference estimators")
  estfun(x$RDDslot$model)
}

#' @S3method bread RDDreg_np
bread.RDDreg_np <- function(x,...){
  inf_met <- infType(x) ## def in Misc.R
  if(inf_met=="se") stop("No 'vcovHC', 'vcovCluster', 'estfun' etc can be applied to RDDrg_np with non-parametric inference estimators")
  bread(x$RDDslot$model)
} 


# sandwich.RDDreg_np <- function (x, bread. = bread, meat. = meat, ...){
#   inf_met <- infType(x) ## def in Misc.R
#   if(inf_met=="se") stop("No 'vcovHC', 'vcovCluster', 'estfun' etc can be applied to RDDrg_np with non-parametric inference estimators")
#   sandwich(x$RDDslot$model, bread.=bread., meat.=meat., ...)
# }

#' @S3method model.frame RDDreg_np
model.frame.RDDreg_np <- function (formula, ...) 
  model.frame.lm(formula$RDDslot$model)

#' Cluster Heteroskedasticity-consistent estimation of the covariance matrix. 
#' 
#' Offer a cluster variant of the usual Heteroskedasticity-consistent 
#' @param object Object of class lm, from which RDDreg also inherits.
#' @param clusterVar The variable containing the cluster attributions. 
#' @return A matrix containing the covariance matrix estimate.
#' @export

vcovCluster   <- function(object, clusterVar){
  M <- length(unique(clusterVar))   
  N <- length(clusterVar)  	        
  K <- getModelRank(object)
  dfc <- (M/(M-1))*((N-1)/(N-K))  
  uj  <- apply(estfun(object),2, function(x) tapply(x, clusterVar, sum))
  dfc*sandwich(object, meat.=crossprod(uj)/N)
}

getModelRank <- function(object,...)
  UseMethod("getModelRank")

getModelRank.default <- function(object,...) object$rank

getModelRank.RDDreg_np <- function(object,...) getModelRank.default(object$RDDslot$model)

if(FALSE){

  library(RDDtools)
  data(Lee2008)

  Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)


  reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
  print(x=reg_para )
  summary(reg_para )

## cluster inference
  set.seed(123)
  nlet <- sort(c(outer(letters, letters, paste, sep="")))
  clusRandom <- sample(nlet[1:60], size=nrow(Lee2008_rdd), replace=TRUE)
  clusterInf(reg_para, clusterVar=clusRandom)

  clusterInf(reg_para, clusterVar=clusRandom, type="HC")

## compare with rdd:
  library(rdd)
  rddest <- RDestimate(y~x, data=Lee2008, bw=30, kernel="rectangular", model=TRUE)
  rddest_2 <- RDestimate2(y~x, data=Lee2008, bw=30, kernel="rectangular", model=TRUE, cluster=clusRandom)
  coef(summary(reg_para))
  coef(summary(rddest$model[[2]]))

  all.equal(clusterInf(reg_para, clusterVar=clusRandom, type="HC")["D", "Std. Error"],rddest_2[["se"]][2])
}