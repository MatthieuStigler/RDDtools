#'Construct RDDdata
#' 
#' Do a "scatterplot bin smoothing"
#' 
#' @param x Forcing variable
#' @param y Output
#' @param z Exogeneous variables
#' @param cutpoint Cutpoint
#' @param labels Additional labels to provide as list (with entries x, y, and eventually vector z)
#' @return Object of class \code{RDDdata}, inheriting from \code{data.frame}
#' @export
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>
#' @examples
#' data(Lee2008)
#' rd<- RDDdata(x=Lee2008$x, y=Lee2008$y, cutpoint=0)
#' rd
#' summary(rd)


RDDdata <- function(y, x, z, cutpoint, labels){

## check args
  hasCovar <- !missing(z)
  if(missing(cutpoint)) stop("Please provide cutpoint")

### Check y, x univariate
  k_y <- NCOL(y)
  k_x <- NCOL(x)

  if(any(!c(k_y, k_x)==1)) stop("y or x should be univariate")

### Check y, x, z same size
  n_y <- NROW(y)
  n_x <- NROW(x)
  n_z <- if(hasCovar) NROW(x) else NULL

  if(any(c(n_y, n_x) != n_z)) stop("y or x should be univariate")

### Check cutpoint
  range_x <- range(x, na.rm=TRUE)
  if(cutpoint<range_x[1] |cutpoint>range_x[2]) stop("Cutpoint outside range of x")

## Check labels
  if(!missing(labels)){
    if(!is.list(labels)) stop("labels should be a list.")
    if(is.null(names(labels)) || !all(names(labels)%in%c("x", "y", "z"))) stop("labels should be a list with components x, and/or y, and/or z")
    if(hasCovar){
      if("z"%in%names(labels) && length(labels$z)!=NCOL(z)) stop("There should be ", NCOL(z), " values (dim of z) for component 'z' in labels")
    }
  } else {
    labels <- list()
  }

#   if(is.null(labels$x)) labels$x <- deparse(substitute(x))
#   if(is.null(labels$y)) labels$y <- deparse(substitute(y))
#   if(hasCova && is.null(labels$z)) labels$z <- if(NCOL(z)==1) names(deparse(substitute(y))

## Assemble data
  RDDdat <- data.frame(x=x, y=y)
  if(hasCovar) {
    RDDdat <- cbind(RDDdat,z)
  } 

## return
  class(RDDdat) <- c("RDDdata", "data.frame")
  attr(RDDdat, "hasCovar") <- hasCovar
  attr(RDDdat, "labels") <- labels
  attr(RDDdat, "cutpoint") <- cutpoint
  RDDdat
}

if(FALSE){

library(RDDtools)
data(Lee2008)

### wrong covariate setting, legitimate warnings:
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0, labels=c("a","bb"))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0, labels=list("a","bb"))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0, labels=list(x="a",u="bb"))

### Covariate setting:
Z <- data.frame(z_con=runif(nrow(Lee2008)), z_dic=factor(sample(letters[1:3], size=nrow(Lee2008), replace=TRUE)))

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=c("a","bb"))

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha"))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha", u="aa"))

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha", z="aa"))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha", z=c("aa", "hj")))



}