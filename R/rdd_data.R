#' Construct rdd_data
#' 
#' Construct the base RDD object, containing x, y and the cutpoint, eventuallay covariates. 
#' 
#' @param x Forcing variable
#' @param y Output
#' @param covar Exogeneous variables
#' @param cutpoint Cutpoint
#' @param labels Additional labels to provide as list (with entries \code{x}, \code{y}, and eventually vector \code{covar}). Unused currently. 
#' @param data A data-frame for the \code{x} and \code{y} variables. If this is provided, 
#' the column names can be entered directly for argument \code{x}, \code{y} and \code{covar}. 
#' For \code{covar}, should be a character vector. 
#' @param z Assignment variable for the fuzzy case. Should be 0/1 or TRUE/FALSE variable. 
#' @details Arguments \code{x}, \code{y} (and eventually \code{covar}) can be either given as:
#'  * vectors (eventually data-frame for \code{covar})
#'  * quote/character when \code{data} is also provided. For multiple \code{covar}, use a vector of characters 
#' @return Object of class \code{rdd_data}, inheriting from \code{data.frame}
#' @author Matthieu Stigler \email{Matthieu.Stigler@@gmail.com}
#' @export
#' @examples
#' data(house)
#' rd <- rdd_data(x=house$x, y=house$y, cutpoint=0)
#' rd2 <- rdd_data(x=x, y=y, data=house, cutpoint=0)
#' 
#' # The print() function is the same as the print.data.frame:
#' rd
#'
#' # The summary() and plot() function are specific to rdd_data
#' summary(rd)
#' plot(rd)
#' 
#' # for the fuzzy case, you need to specify the assignment variable z:
#' rd_dat_fakefuzzy <- rdd_data(x=house$x, y=house$y, 
#'                              z=ifelse(house$x>0+rnorm(nrow(house), sd=0.05),1,0), 
#'                              cutpoint=0)
#' summary(rd_dat_fakefuzzy)
#' @md

rdd_data <- function(y, x, covar, cutpoint, z, labels, data) {
    
    
    ## check args
    type <- ifelse(missing(z), "Sharp", "Fuzzy")
    hasCovar <- !missing(covar)
    if (missing(cutpoint)) 
        stop("Please provide cutpoint")
    covar_nam <- deparse(substitute(covar))
    
    ## Use data in case:
    if (!missing(data)) {
        pf <- parent.frame()
        x <- eval(substitute(x), data, enclos = pf)  # copy from with.default
        y <- eval(substitute(y), data, enclos = pf)  # copy from with.default
        if (hasCovar) {
            ## make sure it's not already a df!?
            class_robust <- try(class(eval(substitute(covar))), silent=TRUE)
            if(inherits(class_robust, "try-error")) class_robust <- "quote"
            
            if(any(c("data.frame", "numeric") %in% class_robust)) {
                covar_df <- covar
            } else {
                ## copy code from subset.data.frame
                nl <- as.list(seq_along(data))
                names(nl) <- names(data)
                covar_index <- eval(substitute(covar), nl, parent.frame())
                
                covar_df <- data[,covar_index, drop=FALSE]  
            }
        }
    }
    
    if (missing(data) & hasCovar) covar_df <- covar 
    
    
    ### Check y, x univariate
    k_y <- NCOL(y)
    k_x <- NCOL(x)
    
    if (any(!c(k_y, k_x) == 1)) 
        stop("y or x should be univariate")
    
    ### Check y, x, z same size
    n_y <- NROW(y)
    n_x <- NROW(x)
    n_covar <- if (hasCovar) 
        NROW(x) else NULL
    
    if (any(c(n_y, n_x) != n_covar)) 
        stop("y or x should be univariate")
    
    ### Check cutpoint
    range_x <- range(x, na.rm = TRUE)
    if (cutpoint < range_x[1] | cutpoint > range_x[2]) 
        stop("Cutpoint outside range of x")
    
    ## Check labels
    if (!missing(labels)) {
        if (!is.list(labels)) 
            stop("labels should be a list.")
        if (is.null(names(labels)) || !all(names(labels) %in% c("x", "y", "covar"))) 
            stop("labels should be a list with components x, and/or y, and/or covar")
        if (hasCovar) {
            if ("covar" %in% names(labels) && length(labels$covar) != NCOL(covar_df)) 
                stop("There should be ", NCOL(covar_df), " values (dim of covar) for component 'covar' in labels")
        }
    } else {
        labels <- list()
    }
    
    # if(is.null(labels$x)) labels$x <- deparse(substitute(x)) if(is.null(labels$y)) labels$y <- deparse(substitute(y))
    # if(hasCova && is.null(labels$covar)) labels$covar <- if(NCOL(covar)==1) names(deparse(substitute(y))
    
    ## Assemble data
    rdd_dat <- data.frame(x = x, y = y)
    if (hasCovar) {
        rdd_dat <- cbind(rdd_dat, covar_df)
        if (NCOL(covar_df) == 1 && is.null(colnames(covar_df))) 
            colnames(rdd_dat)[3] <- covar_nam
    }
    
    if (type == "Fuzzy") {
        rdd_dat <- cbind(rdd_dat, z)
    }
    
    ## return
    class(rdd_dat) <- c("rdd_data", "data.frame")
    attr(rdd_dat, "hasCovar") <- hasCovar
    attr(rdd_dat, "labels") <- labels
    attr(rdd_dat, "cutpoint") <- cutpoint
    attr(rdd_dat, "type") <- type
    
    rdd_dat
}


### Specific subsetting methods

# as.data.frame.rdd_data <- function(x) { subset(x, y> }as.data.frame.default(x)

#' @export
"[.rdd_data" <- function(x, i, ...) {
    attr_x <- attributes(x)
    r <- NextMethod("[", object = as.data.frame(x))
    
    ## keep attributes only if remains a data frame!
    if (inherits(r, "data.frame")) {
        attr_x$row.names <- attr(r, "row.names")
        attr_x$names <- attr(r, "names")
        mostattributes(r) <- attr_x
        attributes(r) <- attributes(r)[match(names(attr_x), names(attributes(r)))]
    }
    # newCla <- class(r) if(any(grepl('rdd_data', newCla))) newCla <- newCla[-grepl('rdd_data', newCla)]
    # print(names(attributes(newCla))) if(!inherits(newCla, 'data.frame')) attr(r, 'class')[which(attr(r,
    # 'class')=='data.frame')] <- newCla
    r
}

#' @export
subset.rdd_data <- function(x, subset, select, drop = FALSE, ...) {
    attr_x <- attributes(x)
    
    ### subset code: start
    if (missing(subset)) 
        r <- TRUE else {
        e <- substitute(subset)
        r <- eval(e, x, parent.frame())
        if (!is.logical(r)) 
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    if (missing(select)) 
        vars <- TRUE else {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        vars <- eval(substitute(select), nl, parent.frame())
    }
    res <- x[r, vars, drop = drop]
    ### subset code: end r <- subset.data.frame(x,...)  r <- NextMethod('subset')
    
    ## keep attributes only if remains a data frame!
    if (inherits(r, "data.frame")) {
        attr_x$row.names <- attr(res, "row.names")
        attr_x$names <- attr(res, "names")
        mostattributes(res) <- attr_x
        attributes(res) <- attributes(res)[match(names(attr_x), names(attributes(res)))]
    }
    res
}

#' @export 
as.data.frame.rdd_data <- function(x, ...) {
    class(x) <- "data.frame"
    attr(x, "hasCovar") <- NULL
    attr(x, "labels") <- NULL
    attr(x, "cutpoint") <- NULL
    x
} 
