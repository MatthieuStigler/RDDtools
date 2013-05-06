


dens_estim <- function(x, point, bw){

  N <- length(x)

  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  isIn_bw_left  <- x>=(point-bw) & x<point
  isIn_bw_right <- x>=point & x<=(point+bw)

  NisIn_bw_left  <- sum(isIn_bw_left, na.rm=TRUE)
  NisIn_bw_right <- sum(isIn_bw_right, na.rm=TRUE)

  res <-(NisIn_bw_left+NisIn_bw_right)/(2*N*bw)
  res
}

dens_estim2 <- function(x, point, bw, kernel="gaussian"){


  if(missing(bw)) bw <-  "bw.SJ"

  d <- density(x, from=point, to=point, n=1, na.rm=TRUE, kernel=kernel)
  d$y
}


var_estim <- function(x,y, point, bw, sides=c("both", "uni")){

  sides <- match.arg(sides)

  N <- length(x)
  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  if(sides=="uni"){
    isIn_bw_left  <- x>=(point-bw) & x<point
    isIn_bw_right <- x>=point & x<=(point+bw)
    var_inh_left  <- var(y[isIn_h1_left], na.rm=TRUE)
    var_inh_right <- var(y[isIn_h1_right], na.rm=TRUE)
    res <- c(var_inh_left, var_inh_right)
  } else {
    isIn_bw  <- x>=(point-bw) & x<point+bw
    var_inh  <- var(y[isIn_bw], na.rm=TRUE)
    res <- var_inh
  }
res
}

var_estim2 <- function(x,y, point, bw, estim=c("NW", "LL"), sides=c("both", "uni")){

  sides <- match.arg(sides)
  estim <- match.arg(estim)
  N <- length(x)
  if(missing(bw)) bw <-  1.84*sd(x)*N^(-1/5)

  if(sides=="uni"){
    isIn_bw_left  <- x>=(point-bw) & x<point
    isIn_bw_right <- x>=point & x<=(point+bw)
    var_inh_left  <- var(y[isIn_h1_left], na.rm=TRUE)
    var_inh_right <- var(y[isIn_h1_right], na.rm=TRUE)
    res <- c(var_inh_left, var_inh_right)
  } else {
    if(estim=="NW"){
      m <- ksmooth(x=x, y=y, bandwidth=bw*2, x.points=point)
      s <- ksmooth(x=x, y=y^2, bandwidth=bw*2, x.points=point)
    } else {
      rang <- c(point - 0.2*sd(x, na.rm=TRUE),point + 0.2*sd(x, na.rm=TRUE)) 
      m <- locpoly(x=x, y=y, bandwidth=bw, range.x=rang, gridsize=200)
      s <- locpoly(x=x, y=y^2, bandwidth=bw, range.x=rang, gridsize=200)
    }
    sh <- s$y - m$y^2
    res <- sh

  }
res
}

if(FALSE){


## small test:
 MC1_df <- gen_MC_IK()

# true val
point <- 0
dbeta((point+1)/2 , shape1=2, shape2=4)*1/2

dens_estim(x=MC1_df$x, point=point, bw=0.1)
dens_estim2(x=MC1_df$x, point=point, bw=0.1)

#### VAR
sqrt(var_estim(x=MC1_df$x, y=MC1_df$y, point=0))
sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0))
sqrt(var_estim2(x=MC1_df$x, y=MC1_df$y, point=0, estim="LL"))

}