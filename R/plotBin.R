#' Bin plotting
#' 
#' Do a 'scatterplot bin smoothing'
#' 
#' @param x Forcing variable
#' @param y Output
#' @param h the bandwidth (defaults to \code{2*sd(runvar)*length(runvar)^(-.5)})
#' @param nbins number of Bins
#' @param cutpoint Cutpoint
#' @param plot Logical. Whether to plot or only returned silently
#' @param type Whether returns the y averages, or the x frequencies
#' @param xlim,cex,main,xlab,ylab Usual parameters passed to plot(), see \code{\link{par}}
#' @param \ldots further arguments passed to plot. 
#' @return Returns silently values
#' @references McCrary, Justin. 
#' @importFrom utils head


plotBin <- function(x, y, h = NULL, nbins = NULL, cutpoint = 0, plot = TRUE, type = c("value", "number"), xlim = range(x, na.rm = TRUE), 
    cex = 0.9, main = NULL, xlab, ylab, ...) {
  
  if(sum(c(is.null(h), is.null(nbins)))!=1) stop("Should provide only one of `h`` or `nbins`")
    
    type <- match.arg(type)
    x_name <- if (missing(xlab)) 
        deparse(substitute(x)) else xlab
    y_name <- if (missing(ylab)) 
        deparse(substitute(y)) else ylab
    
    
    ## Set intervals and midpoints
    min_x <- min(xlim)
    max_x <- max(xlim)
    
    ## set h given nBins
    if (!is.null(nbins)) {
      if(length(nbins)==1){
        h_both <- diff(xlim)/nbins
        
        ## compute actual number of bins
        K0 <- (cutpoint - min_x)/h_both
        K1 <- (max_x -cutpoint )/h_both
        
        ## round number of bins
        nbins <- roundEqual(c(K0, K1))
      } 
      
      ## compute corresponding h_L
      K0 <- nbins[1]
      K1 <- nbins[2]
      h_L <- c(cutpoint - min_x)/K0
      h_R <- c(max_x -cutpoint)/K1
      
    } else if(!is.null(h)) {
      if(length(h)==1){
        h_L <- h_R <- h
      } else {
        h_L <- h[1]
        h_R <- h[2]
      }
      K0 <- ceiling((cutpoint - min_x)/h_L)  # Number of bins on left
      K1 <- ceiling((cutpoint + max_x)/h_R)  # Number of bins on right
    }
    
    ## 
    K <- K0 + K1
    
    ## get bins midpoints, breaks, inspired by # Lee and Lemieux (2010) p. 308
    breaks_L <- cutpoint - (K0 - c(1:K0) + 1) * h_L  
    breaks_H <- cutpoint + c(0:K1) * h_R  
    breaks <- c(breaks_L, breaks_H)
    
    # mid_points
    mid_points_bk <- head(breaks, -1)+diff(breaks)/2
    
    ## compute output (mean of count)
    intervs <- cut(x, breaks = breaks, include.lowest = TRUE)
    if(any(is.na(intervs))) warning("NA intervs...")

    ##
    table_intervs <- table(intervs)
    n_non0_intervs <- sum(table_intervs != 0)
    
    y2 <- switch(type, value = tapply(y, intervs, mean, na.rm = TRUE), 
                 number = table_intervs)
    
    
    ## plot
    if (plot) {
      sub <- paste("h=", paste(round(c(h_L, h_R), 4),collapse="/"), ",\t\tn bins=", K, " (", K0, "/", K1,")", sep = "")
        plot(mid_points_bk, as.numeric(y2), pch = 19, cex = cex, xlab = x_name, ylab = y_name, xlim = xlim, ...)
        title(main = main, sub = sub)
        abline(v = cutpoint, lty = 2)
    }
    
    ## return invisible result
    res <- data.frame(x = mid_points_bk, y = y2)
    invisible(res)
}



## Small utility funciton
roundEqual <- function(x){
  if(isTRUE(all.equal(x[1], x[2]))) {
    r <- c(floor(x[1]), ceiling(x[2]))
  } else {
    r <- round(x)
  }
  r
}


if(FALSE){
  xt <- rnorm(100)
  yt <- 1.2*x+rnorm(100)
  plotBin(x=xt, y=yt)
  plotBin(x=xt, y=yt, h=.05)
  plotBin(x=xt, y=yt, h=c(0.05, 0.06))
  
  pl_nb1 <- plotBin(x=xt, y=yt, nbins=25)
  pl_nb2 <- plotBin(x=xt, y=yt, nbins=c(12, 13))
  pl_nb2
}
