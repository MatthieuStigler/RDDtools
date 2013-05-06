#' Testing for balanced covariates: equality of means with t-test
#' 
#' Tests equality of means by a t-test for each covariate, between the two full groups or around the discontinuity threshold 
#' 
#' @param object object of class RDDdata
#' @param bw a bandwidth
#' @param paired Argument of the \code{\link{t.test}} function: logical indicating whether you want paired t-tests.
#' @param var.equal Argument of the \code{\link{t.test}} function:  logical variable indicating whether to treat the two variances as being equal
#' @param p.adjust Whether to adjust the p-values for multiple testing. Uses the \code{\link{p.adjust}} function
#' @param \ldots currently not used
#' @return A data frame wih
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>




#' @export
covarTest_mean <- function(object, bw=NULL, paired = FALSE, var.equal = FALSE, p.adjust=c("none", "holm", "BH", "BY","hochberg", "hommel", "bonferroni")) 
  UseMethod("covarTest_mean")


#' @S3method covarTest_mean RDDdata
covarTest_mean.RDDdata <- function(object, bw=NULL, paired = FALSE, var.equal = FALSE, p.adjust=c("none", "holm", "BH", "BY","hochberg", "hommel", "bonferroni")) {

  cutpoint <- getCutpoint(object)
  covar <- getCovar(object)
  cutvar <- object$x

  covarTest_mean_low(covar=covar,cutvar=cutvar,cutpoint=cutpoint, bw=bw, paired = paired, var.equal = var.equal, p.adjust=p.adjust)

}

covarTest_mean_low <- function(covar,cutvar, cutpoint, bw=NULL, paired = FALSE, var.equal = FALSE, p.adjust=c("none", "holm", "BH", "BY","hochberg", "hommel", "bonferroni")) {

  p.adjust <- match.arg(p.adjust)

## subset
  if(!is.null(bw)){
    isInH <- cutvar >= cutpoint -bw & cutvar <= cutpoint +bw
    covar <- covar[isInH,]
    cutvar <- cutvar[isInH]
  }
  regime <- cutvar < cutpoint

## Split data
  covar_num <- sapply(covar, as.numeric)

  tests <-apply(covar_num, 2, function(x) t.test(x[regime], x[!regime], paired=paired, var.equal=var.equal))
  tests_vals <- sapply(tests, function(x) c(x[["estimate"]], diff(x[["estimate"]]),x[c("statistic", "p.value")]))

## Adjust p values if required:
  if(p.adjust!="none") tests_vals["p.value",] <- p.adjust(tests_vals["p.value",], method=p.adjust)

## Print results
  res <- t(tests_vals)
  colnames(res)[3] <- "Difference"
  res


}




#' Testing for balanced covariates: equality of distribution
#' 
#' Tests equality of distribution with a Kolmogorov-Smirnov for each covariates, between the two full groups or around the discontinuity threshold 
#' 
#' @param object object of class RDDdata
#' @param h a bandwidth
#' @param exact Argument of the \code{\link{ks.test}} function: NULL or a logical indicating whether an exact p-value should be computed.
#' @param p.adjust Whether to adjust the p-values for multiple testing. Uses the \code{\link{p.adjust}} function
#' @param \ldots currently not used
#' @return A data frame wih
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>

#' @export
covarTest_dis <- function(object, bw,  exact=NULL, p.adjust=c("none", "holm", "BH", "BY","hochberg", "hommel", "bonferroni"))
  UseMethod("covarTest_dis")

#' @S3method covarTest_dis RDDdata
covarTest_dis.RDDdata <- function(object, bw=NULL, exact = FALSE,  p.adjust=c("none", "holm", "BH", "BY","hochberg", "hommel", "bonferroni")) {

  cutpoint <- getCutpoint(object)
  covar <- getCovar(object)
  cutvar <- object$x

  covarTest_dis_low(covar=covar,cutvar=cutvar,cutpoint=cutpoint, bw=bw, exact= exact, p.adjust=p.adjust)

}


covarTest_dis_low <- function(covar,cutvar, cutpoint, bw=NULL, exact=NULL, p.adjust=c("none", "holm", "BH", "BY","hochberg", "hommel", "bonferroni")) {

  p.adjust <- match.arg(p.adjust)

## subset
  if(!is.null(bw)){
    isInH <- cutvar >= cutpoint -bw & cutvar <= cutpoint +bw
    covar <- covar[isInH,]
    cutvar <- cutvar[isInH]
  }
  regime <- cutvar < cutpoint



## Split data
  covar_num <- sapply(covar, as.numeric)

  tests <-apply(covar_num, 2, function(x) ks.test(x[regime], x[!regime], exact=exact))
  tests_vals <- sapply(tests, function(x) x[c("statistic", "p.value")])

## Adjust p values if required:
  if(p.adjust!="none") tests_vals["p.value",] <- p.adjust(tests_vals["p.value",], method=p.adjust)

## Print results
  res <- t(tests_vals)
  res


}


##########################################
###### TODO
##########################################
## -mean: can use t.test for factors? What else? Count test? Warn for character/factors!
## -mean: add multivariate hotelling
## -ks: ok for factors?
## -do qqplot?
## -add methods for regs? Once converted to other objects...
## -add example and bettet output documentation
##
##
##

##########################################
###### TESTS
##########################################

if(FALSE){
library(Hotelling)
library(mvtnorm)

data <- rmvnorm(n=200, mean=c(1,2))
spli <- sample(c(TRUE, FALSE), size=200, replace=TRUE)

a<-hotel.stat(data[spli,],data[!spli,])
a

b<-hotel.test(data[spli,],data[!spli,])
b
b$stats

}




if(FALSE){
library(RDDtools)
data(Lee2008)

Z <- data.frame(z_con=runif(nrow(Lee2008)), z_dic=factor(sample(letters[1:3], size=nrow(Lee2008), replace=TRUE)))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0)


covarTest_mean(object=Lee2008_rdd)
covarTest_dis(object=Lee2008_rdd)



}