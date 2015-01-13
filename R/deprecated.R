
plotPlacebo_OLD<- function(RDDregobject, from, to, by=0.1, level=0.95, same_bw=FALSE){

  object <- RDDregobject
  bw <- getBW(object)
  cutpoint <- getCutpoint(object)
  forc_var <- object$model[,"x^1"]

## set grid:
  if(missing(from)) from <- median(forc_var[forc_var<cutpoint])
  if(missing(to))   to <-   median(forc_var[forc_var>=cutpoint])

  seqi <- sort(c(cutpoint,seq(from=from, to=to, by=by)))
  n_seqi <- length(seqi)

## set matrix for results:
  seq_vals <- matrix(NA, nrow=n_seqi, ncol=4, dimnames=list(seqi, c("LATE", "se", "CI_low", "CI_high")))

## get call:
  object_call <- attr(object, "RDDcall")

## original dataset:
  dat_orig <- eval(object_call$RDDobject)

## run each time:
  for(i in seq_along(seqi)){
    attr(dat_orig, "cutpoint") <- seqi[i]
    bw_reg <- if(same_bw) bw else RDDbw_IK(dat_orig)
    object_new <- RDDreg_np(dat_orig, bw=bw_reg)
    if(!inherits(object_new, "try-error")){
      co <- coef(summary(object_new))[2,, drop=FALSE]
      seq_vals[i,"LATE"] <- co[,1]
      seq_vals[i,"se"] <- co[,2]
    }
  }

## compute intervals:
  probs <- (1 - level)/2
  probs <- c(probs, 1 - probs)
  quants <- qnorm(probs)
  seq_vals[,"CI_low"] <- seq_vals[,"LATE"] +quants[1]*seq_vals[,"se"]
  seq_vals[,"CI_high"] <- seq_vals[,"LATE"] +quants[2]*seq_vals[,"se"]


## plot results:
  ra <- range(seq_vals[,c("CI_low", "CI_high")], na.rm=TRUE)
  plot(seqi, seq_vals[,"LATE"], type="l", ylab="LATE", xlab="Cutpoints", ylim=ra)
  title("Placebo test")

  lines(seqi, seq_vals[,"CI_low"], lty=2)
  lines(seqi, seq_vals[,"CI_high"], lty=2) #
  abline(h=0)
  
## add optim in case: 
  est <- RDDcoef(object)
  points(cutpoint, RDDcoef(RDDregobject), col=2)
  segments(cutpoint,ra[1]-1, cutpoint, est, col="red", lty=2)
  segments(min(seqi,na.rm=TRUE)-1, est, cutpoint, est, col="red", lty=2)

## export (silently) results:
  invisible(seq_vals)
}


plotPlacebo_OTHER_OLD <- function(RDDregobject, from=0.25, to=0.75, by=0.1, level=0.95, same_bw=FALSE, device=c("ggplot", "base")){

  device <- match.arg(device)
  object <- RDDregobject
  bw <- getBW(object)
  cutpoint <- getCutpoint(object)
  forc_var <- getOriginalX(RDDregobject)

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
  seq_vals <- matrix(NA, nrow=n_seqi, ncol=6)
  colnames(seq_vals) <- c("cutpoint", "position", "LATE", "se", "CI_low", "CI_high")
  seq_vals[, "cutpoint"] <- seqi

## get call:
  object_call <- attr(object, "RDDcall")

## original dataset:
  dat_orig <- eval(object_call$RDDobject)

## run each time:
  for(i in seq_along(seqi)){

    ## select sample:
    if(seqi[i]<cutpoint){
      dat_sides <- subset(dat_orig, x<cutpoint)
    } else {
      dat_sides <- subset(dat_orig, x>cutpoint) ## exclude x>cutpoint
    }

    ## change the cutpoint
    attr(dat_sides, "cutpoint") <- seqi[i]

    ## Re-estimate model and eventually bw
    bw_reg <- if(same_bw) bw else RDDbw_IK(dat_sides)
    object_new <- RDDreg_np(dat_sides, bw=bw_reg)

    ## assign results (LATE and se)
    if(!inherits(object_new, "try-error")){
      co <- coef(summary(object_new))[2,, drop=FALSE]
      seq_vals[i,"LATE"] <- co[,1]
      seq_vals[i,"se"] <- co[,2]
    }
  }

## compute intervals:
  probs <- (1 - level)/2
  probs <- c(probs, 1 - probs)
  quants <- qnorm(probs)
  seq_vals[,"CI_low"] <- seq_vals[,"LATE"] +quants[1]*seq_vals[,"se"]
  seq_vals[,"CI_high"] <- seq_vals[,"LATE"] +quants[2]*seq_vals[,"se"]


## plot results:
  # prepare df:
  seq_vals <- as.data.frame(seq_vals)
  seq_vals$position <- ifelse(seq_vals$cutpoint < cutpoint, "left", "right")

  # get estimates at true cutpoint :
  est <- RDDcoef(object)
  est_conf <- confint(RDDregobject, level=level)["D",]

  if(device=="base"){
    ra <- range(seq_vals[,c("CI_low", "CI_high")], est_conf, na.rm=TRUE)
    xlims <- c(quants_left[1], quants_right[2])
#     ylims <- range(seq_vals[, c("LATE", "CI_low", "CI_high")], est_conf)
    plot(seqi_left, seq_vals[1:n_seqi_left,"LATE"], type="l", ylab="LATE", xlab="Cutpoints", ylim=ra, xlim=xlims)
    title("Placebo test")
    abline(h=0)

    # left CI
    lines(seqi_left, seq_vals[1:n_seqi_left,"CI_low"], lty=2)
    lines(seqi_left, seq_vals[1:n_seqi_left,"CI_high"], lty=2) 

    # right values:
    lines(seqi_right, seq_vals[(n_seqi_left+1):n_seqi,"LATE"], lty=1)
    lines(seqi_right, seq_vals[(n_seqi_left+1):n_seqi,"CI_low"], lty=2)
    lines(seqi_right, seq_vals[(n_seqi_left+1):n_seqi,"CI_high"], lty=2)

    # add estimate at true cutoff 
    points(cutpoint, est, col=2)
    segments(cutpoint,ra[1]-1, cutpoint, est, col="red", lty=2)
    segments(min(seqi,na.rm=TRUE)-1, est, cutpoint, est, col="red", lty=2)
  } else {

    est_df <- data.frame(cutpoint=cutpoint, LATE=est, position="middle", CI_low=est_conf[1], CI_high=est_conf[2])

    # hack for decent width of error bar:
    last_left <- nrow(subset(seq_vals, position=="left"))
    W <- diff(seq_vals[c(last_left, last_left+1), "cutpoint"])/5

    pl <- qplot(x=cutpoint, y=LATE, data=seq_vals, geom="line", colour=position)+
	  geom_smooth(aes(ymin=CI_low, ymax=CI_high), data=seq_vals, stat="identity")+
	  theme(legend.position="none")+geom_hline(yintercept=0)+
	  geom_point(aes(x=cutpoint, y=LATE), data=est_df)+
	  geom_errorbar(aes(ymin=CI_low, ymax=CI_high), data=est_df, width=W)
    print(pl)
  }

## export (silently) results:
  invisible(seq_vals)
}
