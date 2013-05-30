#' Generate Monte Carlo simulations of Imbens and Kalyanaraman
#' 
#' Generate the simulations reported in Imbens and Kalyanaraman (2012)
#' @param n The size of sampel to generate
#' @param version The MC version of Imbens and Kalnayaraman (between 1 and 4).
#' @param sd The standard deviation of the error term. 
#' @return An data frame with x and y variables. 
#' @references TODO
#' @export
#' @examples
#' MC1_dat <- gen_MC_IK()
#' MC1_rdd <- RDDdata(y=MC1_dat$y, x=MC1_dat$x, cutpoint=0)
#' ## Use np regression:
#' reg_nonpara <- RDDreg_np(RDDobject=MC1_rdd)
#' reg_nonpara
#' 

gen_MC_IK <- function(n=200, version=1, sd=0.1295){
 
  if(!version%in% c(1:4) |length(version) !=1) stop("arg 'version' should be between 1 and 4")
  foo <- switch(version, 
			"1"=gen_MC_IK_1,
			"2"=gen_MC_IK_2,
			"3"=gen_MC_IK_3,
			"4"=gen_MC_IK_4)
  res <- foo(n=n, sd=sd)
  res
}


####################################
######### MC 1
####################################

gen_MC_IK_1 <- function(n=200,  sd=0.1295){

## Regressor:
  Z <- rbeta(n, shape1=2, shape2=4, ncp = 0)
  X <- 2*Z-1
  error <- rnorm(n, sd=sd)

## Prepare variables:
  Y <- vector("numeric", length=n)
  ind_below <- X<0
  X_low <- X[ind_below]
  X_up  <- X[!ind_below]

## Compute Y variables:
  Y[ind_below]  <- 0.48 +  1.27*X_low + 7.18*X_low^2 + 20.21* X_low^3 +21.54*X_low^4 +7.33*X_low^5 + error[ind_below]
  Y[!ind_below] <- 0.52 +  0.84*X_up - 3*   X_up^2 +  7.99* X_up^3 - 9.01*X_up^4 +3.56*X_up^5 + error[!ind_below]

## Result:
  res <- data.frame(x=X, y=Y)
  return(res)
}

####################################
######### MC 2
####################################

gen_MC_IK_2 <- function(n=200,  sd=0.1295){

## Regressor:
  Z <- rbeta(n, shape1=2, shape2=4, ncp = 0)
  X <- 2*Z-1
  error <- rnorm(n, sd=sd)

## Compute Y variables:
  Y <- ifelse(X<0, 3*X^2, 4*X^2) + error

## Result:
  res <- data.frame(x=X, y=Y)
  return(res)
}

  
####################################
######### MC 3
####################################

gen_MC_IK_3 <- function(n=200,  sd=0.1295){

## Regressor:
  Z <- rbeta(n, shape1=2, shape2=4, ncp = 0)
  X <- 2*Z-1
  error <- rnorm(n, sd=sd)

## Compute Y variables:
  Y <- 0.42 + ifelse(X<0, 0, 0.1) + 0.84*X - 3*X^2 +7.99 * X^3-9.01*X^4+3.56*X^5 + error

## Result:
  res <- data.frame(x=X, y=Y)
  return(res)
}

####################################
######### MC 4
####################################

gen_MC_IK_4 <- function(n=200,  sd=0.1295){

## Regressor:
  Z <- rbeta(n, shape1=2, shape2=4, ncp = 0)
  X <- 2*Z-1
  error <- rnorm(n, sd=sd)

## Compute Y variables:
  Y <- 0.42 + ifelse(X<0, 0, 0.1) + 0.84*X  +7.99 * X^3-9.01*X^4+3.56*X^5 + error

## Result:
  res <- data.frame(x=X, y=Y)
  return(res)
}


####################################
######### MC simple
####################################

gen_MC_simple <- function(n=200, LATE=0.3){

## Regressor:
  x <- rnorm(n)
  D <- x>= 0
  y <- 0.8 + LATE*D+ 0.3*x+0.1*x*D+rnorm(n)
  RDDdata(x=x, y=y, cutpoint=0)

}