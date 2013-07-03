#'Construct RDDdata
#' 
#' Construct the base RDD object, containing x, y and the cutpoint, eventuallay covariates. 
#' 
#' @param x Forcing variable
#' @param y Output
#' @param z Exogeneous variables
#' @param cutpoint Cutpoint
#' @param labels Additional labels to provide as list (with entries x, y, and eventually vector z). Unused currently. 
#' @param data A data-frame for the x and y variables. If this is provided, 
#' the column names can be entered directly for argumetn \code{x} and \code{y}
#' @return Object of class \code{RDDdata}, inheriting from \code{data.frame}
#' @export
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>
#' @examples
#' data(Lee2008)
#' rd<- RDDdata(x=Lee2008$x, y=Lee2008$y, cutpoint=0)
#' rd2 <- RDDdata(x=x, y=y, data=Lee2008, cutpoint=0)
#' 
#' # The print() function is the same as the print.data.frame:
#' rd
#'
#' # The summary() and plot() function are specific to RDDdata
#' summary(rd)
#' plot(rd)


RDDdata <- function(y, x, z, cutpoint, labels, data){

  
## check args
  hasCovar <- !missing(z)
  if(missing(cutpoint)) stop("Please provide cutpoint")

## Use data in case:
  if(!missing(data)){
    pf <- parent.frame()
    x <- eval(substitute(x), data, enclos = pf) # copy from with.default
    y <- eval(substitute(y), data, enclos = pf) # copy from with.default
    if(hasCovar) z <- eval(substitute(z), data, enclos = pf) # idem
  }
  
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


### Specific subsetting methods

#####  @S3method as.data.frame RDDdata
# as.data.frame.RDDdata <- function(x) {
# subset(x, y>
# }as.data.frame.default(x)

#' @S3method "[" RDDdata
'[.RDDdata' <- function(x,i,...){
  attr_x <- attributes(x)
  r <- NextMethod("[", object=as.data.frame(x))

## keep attributes only if remains a data frame!
  if(inherits(r, "data.frame")){
    attr_x$row.names <- attr(r, "row.names")
    attr_x$names <- attr(r, "names")
    mostattributes(r) <- attr_x
    attributes(r) <- attributes(r)[match(names(attr_x), names(attributes(r)))]
  }
#   newCla <- class(r)
#   if(any(grepl("RDDdata", newCla))) newCla <- newCla[-grepl("RDDdata", newCla)]
#   print(names(attributes(newCla)))
# 
#   if(!inherits(newCla, "data.frame")) attr(r, "class")[which(attr(r, "class")=="data.frame")] <- newCla
  r
}

#' @S3method subset RDDdata
subset.RDDdata <- function (x, subset, select, drop = FALSE, ...) {
  attr_x <- attributes(x)

### subset code: start
    if (missing(subset)) 
        r <- TRUE
    else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r)) 
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    if (missing(select)) 
        vars <- TRUE
    else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    res <- x[r, vars, drop = drop]
### subset code: end
#   r <- subset.data.frame(x,...)
#   r <- NextMethod("subset")

## keep attributes only if remains a data frame!
  if(inherits(r, "data.frame")){
    attr_x$row.names <- attr(res, "row.names")
    attr_x$names <- attr(res, "names")
    mostattributes(res) <- attr_x
    attributes(res) <- attributes(res)[match(names(attr_x), names(attributes(res)))]
  }
  res
}

#' @S3method as.data.frame RDDdata
as.data.frame.RDDdata <- function(x,...){
  class(x) <- "data.frame"
  attr(x, "hasCovar") <- NULL
  attr(x, "labels")   <- NULL 
  attr(x, "cutpoint") <- NULL
  x
}


if(FALSE){

library(RDDtools)
data(Lee2008)

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
Lee2008_rdd2 <- RDDdata(y=y, x=x,data=Lee2008, cutpoint=0)

all.equal(Lee2008_rdd, Lee2008_rdd2)

### wrong covariate setting, legitimate warnings:
Lee2008_rdd_lab1 <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0, labels=c("a","bb"))
Lee2008_rdd_lab2 <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0, labels=list("a","bb"))
Lee2008_rdd_lab3 <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0, labels=list(x="a",u="bb"))

### Covariate setting:
Z <- data.frame(z_con=runif(nrow(Lee2008)), z_dic=factor(sample(letters[1:3], size=nrow(Lee2008), replace=TRUE)))

Lee2008_rdd_Z <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=c("a","bb"))

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha"))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha", u="aa"))

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha", z="aa"))
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z, cutpoint=0, labels=list(x="aha", z=c("aa", "hj")))

### subsetting
dat <- Lee2008_rdd
dat_sub <- subset(Lee2008_rdd, x<1000)
dat_ind <- Lee2008_rdd[1:nrow(Lee2008_rdd),]
dat_ind_1 <- Lee2008_rdd[,1]
dat_ind_2 <- Lee2008_rdd[1:5,]


all.equal(dat, dat_sub)
all.equal(attributes(dat), attributes(dat_sub))

all.equal(dat, dat_ind)
all.equal(attributes(dat), attributes(dat_ind))

df<- as.data.frame(Lee2008_rdd)
head(df)


head(Lee2008_rdd_Z)
colnames(Lee2008_rdd_Z[, -c(1,2)])
attributes(Lee2008_rdd_Z[, -c(1,2)])

colnames(subset(Lee2008_rdd_Z,select= c("z1","z2")))

colnames(dat_sub)
colnames(dat_ind)
colnames(dat_ind_1)
colnames(dat_ind_2)
}