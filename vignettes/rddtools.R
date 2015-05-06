## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## ------------------------------------------------------------------------
library(rddtools)
data(Lee2008)

## ------------------------------------------------------------------------
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

## ----dataPlot------------------------------------------------------------
summary(Lee2008_rdd)
plot(Lee2008_rdd)

## ----reg_para------------------------------------------------------------
reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd, order=4)
reg_para

plot(reg_para)

## ----RegPlot-------------------------------------------------------------
bw_ik <- RDDbw_IK(Lee2008_rdd)
reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd, bw=bw_ik)
print(reg_nonpara)
plot(x=reg_nonpara)

## ----SensiPlot-----------------------------------------------------------
plotSensi(reg_nonpara, from=0.05, to=1, by=0.1)

## ----placeboPlot---------------------------------------------------------
plotPlacebo(reg_nonpara)

## ----DensPlot------------------------------------------------------------
dens_test(reg_nonpara)

## ------------------------------------------------------------------------
set.seed(123)
n_Lee <- nrow(Lee2008)
Z <- data.frame(z1 = rnorm(n_Lee, sd=2), 
                z2 = rnorm(n_Lee, mean = ifelse(Lee2008<0, 5, 8)), 
                z3 = sample(letters, size = n_Lee, replace = TRUE))
Lee2008_rdd_Z <- RDDdata(y = Lee2008$y, x = Lee2008$x, covar = Z, cutpoint = 0)

## ------------------------------------------------------------------------
## test for equality of means around cutoff:
covarTest_mean(Lee2008_rdd_Z, bw=0.3)

## Can also use function covarTest_dis() for Kolmogorov-Smirnov test:
covarTest_dis(Lee2008_rdd_Z, bw=0.3)

