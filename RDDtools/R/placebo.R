#' Draw a (density) plot of placebo tests
#' 
#' Draw a plot of placebo tests, estimating the impact on fake cutpoints
#' @param object the output of an RDD regression
#' @param device Whether to draw a base or a ggplot graph.
#' @param \ldots Further arguments passed to specific methods. 
#' @param vcov. Specific covariance function to pass to coeftest. See help of package \code{\link[sandwich]{sandwich}}.
#' @return A data frame containing the cutpoints, their corresponding estimates and confidence intervals. 
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>
#' @examples
#' data(Lee2008)
#' Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
#' reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
#' plotPlacebo(reg_nonpara)
#' 
#' # Use with another vcov function; cluster case
#' reg_nonpara_lminf <- RDDreg_np(RDDobject=Lee2008_rdd, inference="lm")
#' # need to be a function applied to updated object!
#' vc <- function(x) vcovCluster(x, clusterVar=model.frame(x)$x)
#' plotPlacebo(reg_nonpara_lminf, vcov. = vc)


#' @export
plotPlacebo <- function(object, device=c("ggplot", "base"), ...)
  UseMethod("plotPlacebo")

#' @rdname plotPlacebo
#' @method plotPlacebo RDDreg
#' @S3method plotPlacebo RDDreg
#' @param from Starting point of the fake cutpoints sequence. Refers ot the quantile of each side of the true cutpoint
#' @param to Ending   point of the fake cutpoints sequence. Refers ot the quantile of each side of the true cutpoint
#' @param by Increments of the from-to sequence
#' @param level Level of the confidence interval shown
#' @param same_bw Whether to re-estimate the bandwidth at each point
plotPlacebo.RDDreg <- function(object, device=c("ggplot", "base"), from=0.25, to=0.75, by=0.1, level=0.95, same_bw=FALSE, vcov.=NULL, ...){

  device <- match.arg(device)

  # compute Placebos:
  seq_vals <- computePlacebo(object=object, from=from, to=to, by=by, level=level, same_bw=same_bw, vcov.=vcov.)
  
  ## Use low-level to plot:
  plotPlacebo_low(seq_vals, device=device)

  invisible(seq_vals)
}



#' @S3method plotPlacebo PlaceboVals
plotPlacebo.PlaceboVals <- function(object, device=c("ggplot", "base"),...){

  device <- match.arg(device)
  plotPlacebo_low(object, device=device)

  invisible(object)
}


plotPlacebo_low <- function(seq_vals,  device=c("ggplot", "base")){

  device <- match.arg(device)

  if(device=="base"){
    ylims <- range(seq_vals[,c("CI_low", "CI_high")], na.rm=TRUE)
    xlims <- range(seq_vals$cutpoint)

    dat_left <- subset(seq_vals, position=="left")
    dat_right <- subset(seq_vals, position=="right")
    dat_true <- subset(seq_vals, position=="True")

    plot(dat_left$cutpoint, dat_left$LATE, type="l", ylab="LATE", xlab="Cutpoints", ylim=ylims, xlim=xlims)
    title("Placebo test")
    abline(h=0)

    # left CI
    lines(dat_left$cutpoint, dat_left$CI_low, lty=2)
    lines(dat_left$cutpoint, dat_left$CI_high, lty=2) 

    # right values:
    lines(dat_right$cutpoint, dat_right$LATE, lty=1)
    lines(dat_right$cutpoint, dat_right$CI_low, lty=2)
    lines(dat_right$cutpoint, dat_right$CI_high, lty=2)

    # add estimate at true cutoff 
    points(dat_true$cutpoint, dat_true$LATE, col=2)
    segments(dat_true$cutpoint,ylims[1]-1, dat_true$cutpoint, dat_true$LATE, col="red", lty=2) ## vertical line
    segments(xlims[1]-1, dat_true$LATE, dat_true$cutpoint, dat_true$LATE, col="red", lty=2)
  } else {
    seq_vals_placeb <- subset(seq_vals, position!="True")
    seq_vals_true <- subset(seq_vals, position=="True")

    # hack for decent width of error bar:
    last_left <- nrow(subset(seq_vals_placeb, position=="left"))
    W <- diff(seq_vals_placeb[c(last_left, last_left+1), "cutpoint"])/5

    pl <- qplot(x=cutpoint, y=LATE, data=seq_vals_placeb, geom="line", colour=position)+
	  geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=seq_vals_placeb, stat="identity")+
	  theme(legend.position="none")+geom_hline(yintercept=0)+
	  geom_point(aes(x=cutpoint, y=LATE), data=seq_vals_true)+
	  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), data=seq_vals_true, width=W)
    print(pl)
  }

## export (silently) results:
  invisible(seq_vals)
}


#' @rdname plotPlacebo
#' @export
plotPlaceboDens <- function(object, device=c("ggplot", "base"), ...)
  UseMethod("plotPlaceboDens")

#' @rdname plotPlacebo
#' @method plotPlaceboDens RDDreg
#' @S3method plotPlaceboDens RDDreg
plotPlaceboDens.RDDreg <- function(object, device=c("ggplot", "base"), from=0.25, to=0.75, by=0.1, level=0.95, same_bw=FALSE, vcov.=NULL, ...){

  device <- match.arg(device)

  # compute Placebos:
  seq_vals <- computePlacebo(object=object, from=from, to=to, by=by, level=level, same_bw=same_bw, vcov.=vcov.)
  
  ## Use low-level to plot:
  plotPlaceboDens_low(seq_vals, device=device)

  invisible(seq_vals)
}


#' @S3method plotPlaceboDens PlaceboVals
plotPlaceboDens.PlaceboVals <- function(object, device=c("ggplot", "base"), ...){

  device <- match.arg(device)
  plotPlaceboDens_low(object, device=device,...)

  invisible(object)
}


plotPlaceboDens_low <- function(seq_vals,  device=c("ggplot", "base")){

  device <- match.arg(device)
  seq_vals_placeb <- subset(seq_vals, position!="True")
  perc_rejected <- 100*mean(seq_vals_placeb$p_value<0.05)


  if(device=="base") {
    stop("not implemented")
  } else {
    seq_vals_true <- subset(seq_vals, position=="True")

    dens_max <- max(density(seq_vals_placeb$LATE)$y) # not efficient....
    text_rej <- paste("Perc rejected:", perc_rejected, "%")


    pl <- qplot(x=LATE, data=seq_vals_placeb, geom="density")+
	  geom_vline(xintercept=0, lty=2)+geom_vline(xintercept=seq_vals_true$LATE, colour="red")+
	  annotate("text", x = seq_vals_true$LATE,  y = dens_max, label = "LATE at true \ncutpoint ", colour="red", hjust=1)+
	  annotate("text", x = seq_vals_true$LATE,  y = 0, label = text_rej, hjust=1, vjust=1)
    print(pl)
  }

## export (silently) results:
  invisible(seq_vals)
}


#' @rdname plotPlacebo
#' @export computePlacebo


computePlacebo <- function(object, from=0.25, to=0.75, by=0.1, level=0.95, same_bw=FALSE, vcov.=NULL){

  bw <- getBW(object)
  if(!is.null(vcov.)&& !is.function(vcov.)) stop("'arg' vcov. should be a function (so can be updated at each step, not a matrix")
  cutpoint <- getCutpoint(object)
  forc_var <- getOriginalX(object)

## set grid:
  quants_left  <- quantile(forc_var[forc_var<cutpoint], probs=c(from, to))
  quants_right <- quantile(forc_var[forc_var>=cutpoint], probs=c(from, to))

  seqi_left  <- seq(from=quants_left[1], to=quants_left[2], by=by)
  seqi_right <- seq(from=quants_right[1], to=quants_right[2], by=by)
  seqi <- c(seqi_left, seqi_right)

  n_seqi_left <- length(seqi_left)
  n_seqi_right <- length(seqi_right)
  n_seqi <- length(seqi)

## set matrix for results:
  seq_vals <- matrix(NA, nrow=n_seqi, ncol=8)
  colnames(seq_vals) <- c("cutpoint", "position", "LATE", "se", "p_value", "CI_low", "CI_high", "bw")
  seq_vals[, "cutpoint"] <- seqi

## get original call:
  object_call <- getCall(object)

## original dataset:
  dat_orig <- eval(object_call$RDDobject)
  hasCov <- hasCovar(dat_orig)

## run each time:
  for(i in seq_along(seqi)){

    ## select sample
    if(seqi[i]<cutpoint){
      dat_sides <- subset(dat_orig, x<cutpoint)
    } else {
      dat_sides <- subset(dat_orig, x>cutpoint) ## exclude x>cutpoint
    }

    
    ## change the cutpoint, reattribute new data:
    attr(dat_sides, "cutpoint") <- seqi[i]
    object_call$RDDobject <- dat_sides

    ## Change bw if(same_bw=FALSE)
    object_call$bw <- if(!same_bw) RDDbw_IK(dat_sides) else bw
    
    ## Re-estimate model with new cutpoint/bw
    object_new <- eval(object_call) # RDDreg_np(dat_sides, bw=bw_reg)

    ## assign results (LATE and se)
    if(!inherits(object_new, "try-error")){

      seq_vals[i,"LATE"] <- RDDcoef(object_new)
      if(!is.null(vcov.)) {
        co <- coeftest(object_new, vcov.=vcov.)["D",, drop=FALSE]
      } else {
        co <- RDDcoef(object_new, allInfo=TRUE)
      }
      seq_vals[i,"se"] <- co[,"Std. Error"]
      seq_vals[i,"p_value"] <- co[,4]
      seq_vals[i,"bw"] <- getBW(object_new)
      seq_vals[i,c("CI_low", "CI_high")] <- waldci(object_new, level=level, vcov.=vcov.)["D",] ## confint version working with vcov. 
    }
  }


## Add midpoint:
  if(!is.null(vcov.)) {
    true_co <- coeftest(object, vcov.=vcov.)["D",, drop=FALSE]
  } else {
    true_co <- RDDcoef(object, allInfo=TRUE)
  }
  true_confint <- as.numeric(waldci(object, level=level, vcov.=vcov.)["D",])
  true <- data.frame(cutpoint=cutpoint, position="True", LATE=RDDcoef(object), 
		      se=true_co["D","Std. Error"], p_value=true_co["D",4], 
		      CI_low=true_confint[1], CI_high=true_confint[2], bw=bw)


## output
  seq_vals <- as.data.frame(seq_vals)
  seq_vals$position <- ifelse(seq_vals$cutpoint < cutpoint, "left", "right")

  seq_vals <- rbind(seq_vals, true)
  seq_vals <- seq_vals[order(seq_vals$cutpoint),]
  rownames(seq_vals) <- seq_len(nrow(seq_vals))


#   seq_vals$position <- if(seq_vals$cutpoint == cutpoint) "True"

  class(seq_vals) <- c("PlaceboVals", "data.frame")
  return(seq_vals)
}


##########################################
###### TODO
##########################################
## help file
## -choose between functions

if(FALSE){
library(RDDtools)
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

## Regs
reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)

environment(plotPlacebo) <- environment(RDDdata)
pla_lm <- plotPlacebo(reg_para, by=0.05)
head(pla_lm)

pla_np <- plotPlacebo(reg_nonpara, by=0.05)
head(pla_np )

pla_dat <- computePlacebo(reg_nonpara, by=0.05)
head(pla_dat )
plotPlacebo(pla_dat)
plotPlacebo(pla_dat, device="base")


plaDe_lm <- plotPlaceboDens(reg_para, by=0.05)
plotPlaceboDens(pla_dat)

###

## MC simple rdd
x<-runif(1000,-1,1)
cov<-rnorm(1000)
y<-3+2*x+10*(x>=0)+rnorm(1000)


mc_dat <- RDDdata(y=y, x=x, cutpoint=0)

bw_ik <- RDDbw_IK(mc_dat)
mc_reg <- RDDreg_np(mc_dat, bw=bw_ik)

mc_reg_lm <- RDDreg_lm(mc_dat, bw=bw_ik)
mc_reg_np <- RDDreg_np(mc_dat, bw=bw_ik)
waldci(mc_reg_lm)

environment(plotPlacebo) <- environment(RDDdata)

plotPlacebo(mc_reg)
plotPlacebo(mc_reg, from=0.1)
plotPlacebo(mc_reg, device="ggplot")
plotPlacebo(mc_reg, device="ggplot", by=0.05)
plotPlacebo(mc_reg, device="ggplot", from=0.05,by=0.05, to=0.95)

a<-plotPlacebo(mc_reg_lm)
a
RDDtools:::waldci.default(mc_reg_lm)
waldci(mc_reg_np)
plotPlacebo(mc_reg_lm, device="ggplot")



}
