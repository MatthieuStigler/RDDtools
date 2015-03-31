#' Plot the sensitivity to the bandwidth
#' 
#' Draw a plot showing the LATE estimates depending on multiple bandwidths
#' 
#' @param RDDregobject object of a RDD regression, from either \code{\link{RDDreg_lm}} or \code{\link{RDDreg_np}}
#' @param from First bandwidth point. Default value is max(1e-3, bw-0.1)
#' @param to Last bandwidth point. Default value is bw+0.1
#' @param by Increments in the \code{from} \code{to} sequence
#' @param level Level of the confidence interval
#' @param order For parametric models (from \code{\link{RDDreg_lm}}), the order of the polynomial.
#' @param type For parametric models (from \code{\link{RDDreg_lm}}) whether different orders are represented as different colour or as different facets.
#' @param device Whether to draw a base or a ggplot graph.
#' @param output Whether to return (invisibly) the data frame containing the bandwidths and corresponding estimates, or the ggplot object
#' @param plot Whether to actually plot the data. 
#' @param \ldots Further arguments passed to specific methods
#' @return A data frame containing the bandwidths and corresponding estimates and confidence intervals. 
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>
#' @import methods
#' @examples
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' 
#' #Non-parametric estimate 
#' bw_ik <- RDDbw_IK(Lee2008_rdd)
#' reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd, bw=bw_ik)
#' plotSensi(reg_nonpara)
#' plotSensi(reg_nonpara, device="base")
#'
#' #Parametric estimate:
#' reg_para_ik <- RDDreg_lm(RDDobject=Lee2008_rdd, order=4, bw=bw_ik)
#' plotSensi(reg_para_ik)
#' plotSensi(reg_para_ik, type="facet")



###################################
##### plotSensi: function to plot sensitivity to bandwidth
###################################

#' @export
plotSensi <- function(RDDregobject, from, to, by=0.01, level=0.95, output=c("data", "ggplot"), plot=TRUE, ...)
  UseMethod("plotSensi")

#' @rdname plotSensi
#' @export
#' @param vcov. Specific covariance function to pass to coeftest. See help of package \code{\link[sandwich]{sandwich}}
plotSensi.RDDreg_np <- function(RDDregobject, from, to, by=0.05, level=0.95, output=c("data", "ggplot"), plot=TRUE, device=c("ggplot", "base"), vcov.=NULL, ...){

  device <- match.arg(device)
  output <- match.arg(output)
  if(!is.null(vcov.)&& !is.function(vcov.)) stop("'arg' vcov. should be a function (so can be updated at each step, not a matrix")
  if(device=="base"&&output=="ggplot") stop("Arg 'output=ggplot' only relevant for 'device=ggplot'")

  object <- RDDregobject
  bw <- getBW(object)
  est <- RDDcoef(object)

## set grid:
  if(missing(from))  from <- max(1e-3, bw-0.1)
  if(missing(to))  to <- bw+0.1

  seq_bw <- unique(sort(c(bw,seq(from=from, to=to, by=by))))
  n_seq_bw <- length(seq_bw)

## set matrix for results:
  seq_vals <- matrix(NA, nrow=n_seq_bw, ncol=6)
  colnames(seq_vals) <- c("bw", "LATE", "se", "p_value", "CI_low", "CI_high")
  seq_vals[,"bw"] <- seq_bw

## get call:
  object_call <- getCall(object)

## run each time:
  for(i in seq_along(seq_bw)){
    object_call$bw <- seq_bw[i]
    object_new <- try(eval(object_call), silent=TRUE)
    if(!inherits(object_new, "try-error")){
      seq_vals[i,"LATE"] <- RDDcoef(object_new)
      if(!is.null(vcov.)) {
	co <- coeftest(object_new, vcov.=vcov.)["D",, drop=FALSE]
      } else {
	co <- RDDcoef(object_new, allInfo=TRUE)
      }
      seq_vals[i,"se"] <- co[,"Std. Error"]
      seq_vals[i,"p_value"] <- co[,4]
      seq_vals[i,c("CI_low", "CI_high")] <- waldci(object_new, level=level, vcov.=vcov.)["D",] ## confint version working with vcov. 
    }
  }


## plot results:
  seq_vals <- as.data.frame(seq_vals)
  if(device=="base" && plot){
    ra <- range(seq_vals[,c("CI_low", "CI_high")], na.rm=TRUE)
    plot(seq_vals[,"bw"], seq_vals[,"LATE"], type="l", ylab="LATE", xlab="bandwidth", ylim=ra)
    title("Sensitivity to bandwidth choice")
    lines(seq_bw, seq_vals[,"CI_low"], lty=2)
    lines(seq_bw, seq_vals[,"CI_high"], lty=2) #

    
    ## add optim in case: 
    points(bw, est, col="red")
    segments(bw,0, bw, est, col="red", lty=2)
    segments(0,est, bw, est, col="red", lty=2)
  } else {
    sensPlot <- qplot(x=bw, y=LATE, data=seq_vals, geom="line")
    sensPlot <- sensPlot+ geom_smooth(aes(ymax = CI_high, ymin=CI_low),data=seq_vals, stat="identity") # add the conf int
    point.df <- data.frame(bw=bw, LATE=est)
    sensPlot <- sensPlot + geom_point(data=point.df) # add the conf int
    sensPlot <- sensPlot + geom_vline(xintercept=0, lty=2)
    if(plot) print(sensPlot)
  }

## export (silently) results:
  out <- switch(output, "data"=seq_vals, "ggplot"=sensPlot)
  invisible(out)
}












#' @rdname plotSensi
#' @export
plotSensi.RDDreg_lm <- function(RDDregobject, from, to, by=0.05, level=0.95, output=c("data", "ggplot"), plot=TRUE, order, type=c("colour", "facet"),  ...){

  type <- match.arg(type)
  output <- match.arg(output)
  object <- RDDregobject
  est <- RDDcoef(object)
  bw <- getBW(object)
  origOrder <- getOrder(object)
  hasBw <- !is.null(bw)
  if(!hasBw&type=="facet") stop("Arg 'type=facet' works only when the parametric regression was estimated with a bandwidth")

## set grid:
  if(hasBw){
    if(missing(from))  from <- max(1e-3, bw-0.1)
    if(missing(to))  to <- bw+0.1

    seq_bw <- unique(sort(c(bw,seq(from=from, to=to, by=by))))
    n_seq_bw <- length(seq_bw)
  } else {
    if(!all(c(missing(from), missing(to)))) warning("Args 'from' and 'to' not considered since original input has no bw")
    n_seq_bw <- 1
    seq_bw <- NULL
  }

  if(missing(order)) order <- 0:(getOrder(RDDregobject)+2)
  seq_ord <- order
  n_seq_ord <- length(seq_ord)

## set matrix for results:
  seq_vals <- matrix(NA, nrow=n_seq_bw*n_seq_ord, ncol=6)
  colnames(seq_vals) <- c("bw", "order", "LATE", "se", "CI_low", "CI_high")

## get call:
  object_call <- attr(object, "RDDcall")

## guess if obtained with IKbandwidth? (trick: call$bw would be empty)
#   is_IKband <- is.null(object_call$bw)

## run each time:
  for(j in 1:length(seq_ord)){
    for(i in 1:n_seq_bw){
      # assign new order/bw, and estimate:
      object_call$bw <- seq_bw[i]
      object_call$order <- seq_ord[j]
      object_new <- try(eval(object_call), silent=TRUE)

      # put parameters bw/order into matrix:
      seq_vals[i+(j-1)*n_seq_bw,"bw"] <- if(is.null(seq_bw[i])) NA else seq_bw[i]
      seq_vals[i+(j-1)*n_seq_bw,"order"] <- seq_ord[j]

      # put output estim/se into matrix:
      if(!inherits(object_new, "try-error")){
        co <- RDDcoef(object_new, allInfo=TRUE)
        seq_vals[i+(j-1)*n_seq_bw,"LATE"] <- co[,1]
        seq_vals[i+(j-1)*n_seq_bw,"se"] <- co[,2]
      } else {
        warning("Problem evaluating model with new bw=", 
                object_call$bw, " and new order=",object_call$order, ".")
      }
    }
  }



## compute intervals:
  probs <- (1 - level)/2
  probs <- c(probs, 1 - probs)
  quants <- qnorm(probs)
  seq_vals[,"CI_low"] <- seq_vals[,"LATE"] +quants[1]*seq_vals[,"se"]
  seq_vals[,"CI_high"] <- seq_vals[,"LATE"] +quants[2]*seq_vals[,"se"]


## plot results:
  seq_vals_df <- as.data.frame(seq_vals)
  rownames(seq_vals_df) <- 1:nrow(seq_vals_df)
  if(hasBw) seq_vals_df$order <- as.factor(seq_vals_df$order)


  if(type=="colour"){
    if(hasBw){
      est_point <- data.frame(bw=bw, LATE=est, order=as.factor(origOrder))
      sensPlot <- qplot(x=bw, y=LATE, data=seq_vals_df, colour=order, geom="line")+
        geom_point(data=est_point)+
        geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=seq_vals_df, stat="identity")
    } else {
      est_point <- data.frame(LATE=est, order=origOrder)
      sensPlot <- qplot(x=order, y=LATE, data=seq_vals_df, geom="line")+
        geom_point(data=est_point)+
        geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=seq_vals_df, stat="identity")
    }
  } else {
    sensPlot <- qplot(x=bw, y=LATE, data= seq_vals_df, geom="line")+facet_grid(order~.)+
	  geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=seq_vals_df, stat="identity")
  }

  if(plot) print(sensPlot)


#   if(n_seq_ord==1){
#     ra <- range(seq_vals[,c("CI_low", "CI_high")], na.rm=TRUE)
#     plot(seq_bw, seq_vals[,"LATE"], type="l", ylab="LATE", xlab="bandwidth", ylim=ra)
#     title("Sensitivity to order choice")
#     lines(seq_bw, seq_vals[,"CI_low"], lty=2)
#     lines(seq_bw, seq_vals[,"CI_high"], lty=2) #
#   } else {
#       ra <- range(seq_vals[,c("CI_low", "CI_high")], na.rm=TRUE)
#       for(i in 1:n_seq_ord){
# 	if(i==1) {
# 	  plot(seq_bw, seq_vals[(1:n_seq_bw)+(i-1)*n_seq_bw,"LATE"], type="l", ylab="LATE", xlab="bandwidth", ylim=ra, col=i)
# 	} else {
# 	  lines(seq_bw, seq_vals[(1:n_seq_bw)+(i-1)*n_seq_bw,"LATE"], col=i)
# 	}
# 	title("Sensitivity to order choice")
# 	lines(seq_bw, seq_vals[(1:n_seq_bw)+(i-1)*n_seq_bw,"CI_low"], lty=2, col=i)
# 	lines(seq_bw, seq_vals[(1:n_seq_bw)+(i-1)*n_seq_bw,"CI_high"], lty=2, col=i)
#       }
#   }
  
## add optim in case: 
#   if(is_IKband) {
#     points(object$bw, object$est, col="red")
#     segments(object$bw,0, object$bw, object$est, col="red", lty=2)
#     segments(0,object$est, object$bw, object$est, col="red", lty=2)
# }

## export (silently) results:
  out <- switch(output, "data"=seq_vals_df, "ggplot"=sensPlot)
  invisible(out)
}



##########################################
###### TODO
##########################################
## -plotSensi lm: work when no bandwidth!!


if(FALSE){

library(RDDtools)
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)


reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
reg_para2 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=2)

bw_ik <- RDDbw_IK(Lee2008_rdd)
reg_para_ik2 <- RDDreg_lm(RDDobject=Lee2008_rdd, bw=bw_ik, order=2)
reg_para_ik3 <- RDDreg_lm(RDDobject=Lee2008_rdd, bw=bw_ik, order=3)

plotSensi(reg_para)
plotSensi(reg_para_ik2)
plotSensi(reg_para_ik2, type="facet") 
plotSensi(reg_nonpara)
plotSensi(reg_nonpara, device="base")

plo_res <- plotSensi(RDDregobject=reg_para_ik2, order=1:4)



## extract matrix:
plotSensi.RDDreg_lm(RDDregobject=reg_para_ik2, order=1:4)

a <- plotSensi(RDDregobject=reg_para_ik2, order=1:4, type="facet")
library(ggplot2)



environment(plotSensi.RDDreg_lm) <- environment(RDDdata)
plotSensi(reg_para)

}

