#' Construct RDDdata
#' 
#' Construct the base RDD object, containing x, y and the cutpoint, eventuallay covariates. 
#' 
#' @param x Forcing variable
#' @param y Output
#' @param covar Exogeneous variables
#' @param cutpoint Cutpoint
#' @param labels Additional labels to provide as list (with entries \code{x}, \code{y}, and eventually vector \code{covar}). Unused currently. 
#' @param data A data-frame for the \code{x} and \code{y} variables. If this is provided, 
#' the column names can be entered directly for argument \code{x} and \code{y}
#' @param z Assignment variable for the fuzzy case. 
#' @return Object of class \code{RDDdata}, inheriting from \code{data.frame}
#' @export
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


RDDdata <- function(y, x, covar, cutpoint, z, labels, data){

  
## check args
  type <- ifelse(missing(z), "Sharp", "Fuzzy")
  hasCovar <- !missing(covar)
  if(missing(cutpoint)) stop("Please provide cutpoint")
  covar_nam <- deparse(substitute(covar))

## Use data in case:
  if(!missing(data)){
    pf <- parent.frame()
    x <- eval(substitute(x), data, enclos = pf) # copy from with.default
    y <- eval(substitute(y), data, enclos = pf) # copy from with.default
    if(hasCovar) covar <- eval(substitute(covar), data, enclos = pf) # idem
  }
  
### Check y, x univariate
  k_y <- NCOL(y)
  k_x <- NCOL(x)

  if(any(!c(k_y, k_x)==1)) stop("y or x should be univariate")

### Check y, x, z same size
  n_y <- NROW(y)
  n_x <- NROW(x)
  n_covar <- if(hasCovar) NROW(x) else NULL

  if(any(c(n_y, n_x) != n_covar)) stop("y or x should be univariate")

### Check cutpoint
  range_x <- range(x, na.rm=TRUE)
  if(cutpoint<range_x[1] |cutpoint>range_x[2]) stop("Cutpoint outside range of x")

## Check labels
  if(!missing(labels)){
    if(!is.list(labels)) stop("labels should be a list.")
    if(is.null(names(labels)) || !all(names(labels)%in%c("x", "y", "covar"))) stop("labels should be a list with components x, and/or y, and/or covar")
    if(hasCovar){
      if("covar"%in%names(labels) && length(labels$covar)!=NCOL(covar)) stop("There should be ", NCOL(covar), " values (dim of covar) for component 'covar' in labels")
    }
  } else {
    labels <- list()
  }

#   if(is.null(labels$x)) labels$x <- deparse(substitute(x))
#   if(is.null(labels$y)) labels$y <- deparse(substitute(y))
#   if(hasCova && is.null(labels$covar)) labels$covar <- if(NCOL(covar)==1) names(deparse(substitute(y))

## Assemble data
  RDDdat <- data.frame(x=x, y=y)
  if(hasCovar) {
    RDDdat <- cbind(RDDdat,covar)
    if(NCOL(covar)==1 && is.null(colnames(covar))) colnames(RDDdat)[3] <- covar_nam
  } 

  if(type=="Fuzzy"){
    RDDdat <- cbind(RDDdat,z)
  }

## return
  class(RDDdat) <- c("RDDdata", "data.frame")
  attr(RDDdat, "hasCovar") <- hasCovar
  attr(RDDdat, "labels") <- labels
  attr(RDDdat, "cutpoint") <- cutpoint
  attr(RDDdat, "type") <- type

  RDDdat
}


### Specific subsetting methods

# as.data.frame.RDDdata <- function(x) {
# subset(x, y>
# }as.data.frame.default(x)

#' @export
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

#' @export
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

#' @export 
as.data.frame.RDDdata <- function(x,...){
  class(x) <- "data.frame"
  attr(x, "hasCovar") <- NULL
  attr(x, "labels")   <- NULL 
  attr(x, "cutpoint") <- NULL
  x
}
