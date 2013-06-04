


dens_estim <- function(x, point, bw, eachSide=TRUE){

  N <- length(x)

  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  if(eachSide){
    isIn_bw_left  <- x>=(point-bw) & x<point
    isIn_bw_right <- x>=point & x<=(point+bw)

    NisIn_bw_left  <- sum(isIn_bw_left, na.rm=TRUE)
    NisIn_bw_right <- sum(isIn_bw_right, na.rm=TRUE)

    res <-(NisIn_bw_left+NisIn_bw_right)/(2*N*bw)
  } else {
    isIn_bw_both  <- x>=(point-bw) & x<=(point+bw)
    NisIn_bw_both  <- sum(isIn_bw_both, na.rm=TRUE)
    res <- NisIn_bw_both/(2*N*bw)
  }
  res
}

dens_estim2 <- function(x, point, bw, kernel="gaussian",...){


  if(missing(bw)) bw <-  "SJ"

  d <- density(x, from=point, to=point, n=1, na.rm=TRUE, kernel=kernel, bw=bw,...)
  d$y
}


var_estim <- function(x,y, point, bw, eachSide=TRUE){


  N <- length(x)
  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  if(eachSide){
    isIn_bw_left  <- x>=(point-bw) & x<point
    isIn_bw_right <- x>=point & x<=(point+bw)
    var_inh_left  <- var(y[isIn_bw_left], na.rm=TRUE)
    var_inh_right <- var(y[isIn_bw_right], na.rm=TRUE)
    res <- c(var_inh_left, var_inh_right)
  } else {
    isIn_bw  <- x>=(point-bw) & x<=point+bw
    var_inh  <- var(y[isIn_bw], na.rm=TRUE)
    res <- var_inh
  }
res
}

var_estim2 <- function(x,y, point, bw, estim=c("var", "NW", "NW_loc", "LL_kern", "LL_loc", "var_loc"), sides=c("both", "uni"), kernel=c("Normal", "Uniform"), dfadj=TRUE){

  sides <- match.arg(sides)
  estim <- match.arg(estim)
  kernel <- match.arg(kernel)
  N <- length(x)
  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  if(sides=="uni"){
    isIn_bw_left  <- x>=(point-bw) & x<point
    isIn_bw_right <- x>=point & x<=(point+bw)
    var_inh_left  <- var(y[isIn_bw_left], na.rm=TRUE)
    var_inh_right <- var(y[isIn_bw_right], na.rm=TRUE)
    res <- c(var_inh_left, var_inh_right)
  } else  {
    if(estim=="NW"){
      ker <- switch(kernel, "Uniform"="box", "Normal"="normal")
      m <- ksmooth(x=x, y=y, bandwidth=bw*2, x.points=point, kernel=ker)$y
      s <- ksmooth(x=x, y=y^2, bandwidth=bw*2, x.points=point, kernel=ker)$y
    } else if(estim=="NW_loc"){
      ker <- switch(kernel, "Uniform"=uniK, "Normal"=gaussK)
      df_xy <- data.frame(y=y, x=x, y2=y^2)
#       a <<- locCteSmootherC(x=x, y=y, xeval=point, bw=bw, kernel=uniK)
#       aa <<- locCteSmootherC(x=x, y=y, xeval=point, bw=bw, kernel=gaussK)
      m <- locpol(y~x,data=df_xy, bw=bw, xeval=point, deg=0, kernel=ker)
      s <- locpol(y2~x,data=df_xy, bw=bw, xeval=point, deg=0, kernel=ker)
      m <- m$lpFit["y"]
      s <- s$lpFit["y2"]
    } else if(estim=="LL_kern"){
      if(kernel!="Normal") warning("Kernel set to Normal for locpoly")
      m <- locpoly(x=x, y=y, bandwidth=bw, gridsize=200)
      s <- locpoly(x=x, y=y^2, bandwidth=bw, gridsize=200)
      m <- m$y[which.min(abs(m$x-point))]
      s <- s$y[which.min(abs(s$x-point))]
    } else if(estim=="LL_loc"){
      ker <- switch(kernel, "Uniform"=uniK, "Normal"=gaussK)
      df_xy <- data.frame(y=y, x=x, y2=y^2)
      m <- locpol(y~x,data=df_xy, bw=bw, xeval=point, kernel=ker)
      s <- locpol(y2~x,data=df_xy, bw=bw, xeval=point, kernel=ker)
      m <- m$lpFit["y"]
      s <- s$lpFit["y2"]
    } else {
      s <- m <- 1
    }
    sh <- s - m^2
    res <- sh
    if(estim=="var_loc"){
      ker <- switch(kernel, "Uniform"=uniK, "Normal"=gaussK)
      df_xy <- data.frame(y=y, x=x, y2=y^2)
      m <- locpol(y~x,data=df_xy, bw=bw, xeval=point, kernel=ker)
      res <- m$lpFit$var
    } else if(estim=="var"){
      isIn_bw<- x>=(point-bw) & x<=(point+bw)
      var  <- var(y[isIn_bw], na.rm=TRUE)
      res <- if(dfadj) var*(sum(isIn_bw)-1)/sum(isIn_bw) else var
    }

  }
  names(res) <- NULL
as.numeric(res)
}


## Formula: \sqrt[ (C_2 * \sigma(x)^2 / f(x)) / ( n * h) ]
## Imbens & Kalyan: C_2/N*h (sigma_l^2 + \sigma_r^2)/f(x)
## value of constant: 4.8 (using boundary kernel: Triangular
## (value of constant: 33.6 (using boundary kernel: Triangular
## library(locpol)
## computeRK(equivKernel(TrianK, nu=0, deg=1, lower=0, upper=1), lower=0, upper=Inf)
## or: 
## computeRK(equivKernel(TrianK, nu=0, deg=1, lower=-1, upper=1), lower=-Inf, upper=Inf)

all_var_low <- function(x,y, point, bw, eachSide=TRUE, return=c("se", "all")){

  return <- match.arg(return)

  N <- length(x)
  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  var <- var_estim(x=x, y=y, point=point, bw=bw, eachSide=eachSide)
  dens <- dens_estim(x=x, point=point, bw=bw, eachSide=eachSide)

  C2 <- if(eachSide) 4.8 else 2/3
  const <- C2/(N*bw)
  all <- const*sum(var)/dens
  res <- sqrt(all)
  names(res) <- "se"
  if(return=="all") res <- c(res, cons=const, dens=dens, var=sum(var))
  res

}


all_var <- function(...) all_var_low(...)

all_var.RDDreg.np <- function(x){

  bw <- getBW(x)
  dat <- getOriginalData(x)
  cutpoint <- getCutpoint(x)
  res <- all_var_low(dat$x,dat$y, point=cutpoint, bw=bw, eachSide=TRUE, return="se")
  res
}




####################################################################################
############################
####################################################################################

if(FALSE){

  library(KernSmooth)
  library(RDDtools)
  library(locpol)
  if(packageVersion("locpol")<=0.6) stop("Should get latest dev version of locpol")


environment(all_var.RDDreg.np) <- environment(RDDdata)
  ## small test:
  MC1_df <- gen_MC_IK()

  # true val
  point <- 0
  dbeta((point+1)/2 , shape1=2, shape2=4)*1/2

  dens_estim(x=MC1_df$x, point=point, bw=0.1)
  dens_estim(x=MC1_df$x, point=point)
  dens_estim2(x=MC1_df$x, point=point, bw=0.1)
  dens_estim2(x=MC1_df$x, point=point)

## should correspond?
  dens_estim(x=MC1_df$x, point=point, bw=0.1, eachSide=FALSE)
  dens_estim2(x=MC1_df$x, point=point, bw=0.1, kernel="rectangular")
  d <- density(x=MC1_df$x, bw=0.1, kernel="rectangular")
  d$y[which.min(abs(d$x-point))]
  density(x=MC1_df$x, from=0, to=0, n=1,bw=0.1, kernel="rectangular")$y

  #### VARiance
  sqrt(var_estim(x=MC1_df$x, y=MC1_df$y, point=0))

  sqrt(var_estim(x=MC1_df$x, y=MC1_df$y, point=0, eachSide=FALSE))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="var"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="NW_loc", kernel="Uniform"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="NW", kernel="Uniform"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="NW_loc",kernel="Normal"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="LL_kern"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="LL_loc"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="LL_loc", kernel="Uniform"))
  sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="var_loc"))



  all_var(x=MC1_df$x, y=MC1_df$y, point=0)

  ### test:
  library(RDDtools)
  

  MC1_df_rdd <- RDDdata(x=MC1_df$x, y=MC1_df$y, cutpoint=0)

  bw_ik <- RDDbw_IK(MC1_df_rdd)
  RDD_est <- RDDreg_np(MC1_df_rdd, bw=bw_ik)
  RDD_est_lmnp <- RDDreg_lm(MC1_df_rdd, weights=dnorm(MC1_df_rdd$x, sd=bw_ik))

all_var.RDDreg.np(x=RDD_est)

  ## with np:
  library(np)
  MC1_df_D <- data.frame(MC1_df, D=ifelse(MC1_df$x>=0, 1, 0), Dx=ifelse(MC1_df$x>=0, MC1_df$x, 0))
  bw_ik.np <- npregbw(bws=bw_ik, formula=y~x, data= MC1_df, bandwidth.compute=FALSE, regtype = "ll")
  bw_ik.np_D <- npregbw(bws=rep(bw_ik,3), formula=y~x+D+Dx, data= MC1_df_D, bandwidth.compute=FALSE, regtype = "ll",
			eval=data.frame(x=c(0,0), D=c(0,1), Dx=c(0,0)))
  bw_ik.np_D_mixed <- npregbw(bws=c(bw_ik,0.49,bw_ik), formula=y~x+factor(D)+Dx, data= MC1_df_D, bandwidth.compute=FALSE, regtype = "ll",
			eval=data.frame(x=c(0,0), D=c(0,1), Dx=c(0,0)))

  model.np <- npreg(bw_ik.np, exdat=0)
  model.np_D <- npreg(bw_ik.np_D, exdat=data.frame(x=0,D=0, Dx=0))
  model.np_D_mix <- npreg(bw_ik.np_D_mixed)
  model.np_left <- npreg(npregbw(bws=bw_ik, formula=y~x, data= subset(MC1_df,x<0), bandwidth.compute=FALSE, regtype = "ll"))
  model.np_right <- npreg(npregbw(bws=bw_ik, formula=y~x, data= subset(MC1_df,x>=0), bandwidth.compute=FALSE, regtype = "ll"))


  pred_np <- predict(model.np, newdata=data.frame(x=0), se.fit=TRUE)
  pred_np_D0 <- predict(model.np_D, newdata=data.frame(x=0, D=0, Dx=0), se.fit=TRUE)
  pred_np_mix_D0 <- predict(model.np_D_mix, newdata=data.frame(x=0, D=factor(0), Dx=0), se.fit=TRUE)
  pred_np_D1 <- predict(model.np_D, newdata=data.frame(x=0, D=1, Dx=0), se.fit=TRUE)
  pred_np_mix_D1 <- predict(model.np_D_mix, newdata=data.frame(x=0, D=factor(1), Dx=0), se.fit=TRUE)
  pred_np_D1$fit -pred_np_D0$fit
    
  pred_left <- predict(model.np_left, newdata=data.frame(x=0), se.fit=TRUE)
  pred_right <- predict(model.np_right, newdata=data.frame(x=0), se.fit=TRUE)

  pred_li <- list(pred_np=pred_np, pred_left=pred_left, pred_right=pred_right, 
		  pred_np_D0=pred_np_D0, pred_np_D1=pred_np_D1,
		  pred_np_D0_mix=pred_np_mix_D0, pred_np_D1_mix=pred_np_mix_D1)
  sapply(pred_li, function(x) c(fit=x$fit, se.fit=x$se.fit))

  pred_right$fit-pred_left$fit

  summary(RDD_est )

## get same result with RDDreg_lm: 
  com_vals <-rbind(
		  left_point=c(RDD=coef(summary(RDD_est_lmnp))[1,1], np_1side=pred_left$fit, np_D0=pred_np_D0$fit),
		  left_point_se=c(RDD=coef(summary(RDD_est_lmnp))[1,2], np_1side=pred_left$se.fit, np_D0=pred_np_D0$se.fit),
		  right_point=c(RDD=sum(coef(summary(RDD_est_lmnp))[1:2,1]), np_1side=pred_right$fit, np_D1=pred_np_D1$fit),
		  right_point_se=c(RDD=sum(coef(summary(RDD_est_lmnp))[1:2,2]), np_1side=pred_right$se.fit, np_D1=pred_np_D1$se.fit),
		  diff=c(RDD=coef(summary(RDD_est_lmnp))[2,1], np_1side=pred_np_D1$fit -pred_np_D0$fit, np_D1=NA)
		  )
com_vals 
  coef(summary(RDD_est_lmnp))[2,1]

a<-plot(model.np_D, plot.errors.method="bootstrap", plot.behavior="plot-data", plot.errors.style="bar")#, plot.errors.center="bias")
str(a)
head(a$r2$eval)
head(a$r1$eval)

## with liblocpol
  library(locpol)
  library(devtools)


  model.liblocpol_both <- locpol(y~x, data=MC1_df, kernel=gaussK, xeval=0, bw=bw_ik, bwVar=1.2)
  model.liblocpol_both_triK <- locpol(y~x, data=MC1_df, kernel=TrianK, xeval=0, bw=bw_ik, bwVar=1.2)
  model.liblocpol_left <- locpol(y~x, data=subset(MC1_df,x<0), kernel=gaussK, xeval=0, bw=bw_ik, bwVar=1.2)
  model.liblocpol_left_a <- locpol(y~x, data=subset(MC1_df,x<0), kernel=gaussK, xeval=0, bw=bw_ik, bwVar=1)
  model.liblocpol_right <- locpol(y~x, data=subset(MC1_df,x>=0), kernel=gaussK, xeval=0, bw=bw_ik)
  model.liblocpol_right_triK <- locpol(y~x, data=subset(MC1_df,x>=0), kernel=TrianK, xeval=0, bw=bw_ik)

  model_locpol_li <- list(liblocpol_both=model.liblocpol_both, 
			  liblocpol_left=model.liblocpol_left, 
			  liblocpol_left_a=model.liblocpol_left_a,
			  liblocpol_right=model.liblocpol_right)

se.locpol <- function(x) sqrt(x$CIwidth * x$lpFit$var/x$lpFit$xDen)

## Compare se of np and locpol on full, left and right:
round(sapply(model_locpol_li, function(x) c(fit=fitted(x), se.fit=se.locpol(x))),9)
round(sapply(pred_li, function(x) c(fit=x$fit, se.fit=x$se.fit)),9)


## Compare se of np and locpol on full:
  a<- all_var(x=MC1_df$x, y=MC1_df$y, point=0, bw=bw_ik, return="all")
  aa<- all_var(x=MC1_df$x, y=MC1_df$y, point=0, bw=bw_ik, eachSide=FALSE, return="all")
  loc_right <- c(se.locpol(model.liblocpol_right_triK), model.liblocpol_right_triK$CIwidth, model.liblocpol_right_triK$lpFit$xDen,model.liblocpol_right_triK$lpFit$var)
  loc_both <- c(se.locpol(model.liblocpol_both_triK), model.liblocpol_both_triK$CIwidth, model.liblocpol_both_triK$lpFit$xDen,model.liblocpol_both_triK$lpFit$var)

pred_np
model.np$merr

rbind(a, loc_right, aa, loc_both)

computeRK(equivKernel(TrianK, nu=0, deg=1, lower=0, upper=1), lower=0, upper=Inf)/(nrow(MC1_df)*bw_ik)
computeRK(equivKernel(TrianK, nu=0, deg=1, lower=-1, upper=1), lower=-Inf, upper=Inf)/(nrow(MC1_df)*bw_ik)

}