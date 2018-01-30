library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
rd_dat <- rdd_data(x=x, y=y, data=house, cutpoint=0)
reg_lm <- rdd_reg_lm(rd_dat)
reg_np <- rdd_reg_np(rd_dat)
reg_dat <- list(rd=rd_dat, reg_np=reg_np)

## plots
plot(rd_dat)
plot(reg_np)
plot(reg_lm)

## IK
mapply(function(kernel) rdd_bw_ik(rd_dat, kernel=kernel), kernel = c("Triangular", "Uniform", "Normal"))

## CCT estim
rdd_bw_cct_estim_ARG_1 <-  c("mserd", "msetwo", "msesum", "msecomb1", "msecomb2",
                             "cerrd", "certwo", "cersum", "cercomb1")
rdd_bw_cct_estim_ARG_2 <-  c("Triangular", "Uniform", "Epanechnikov")
mapply(function(arg1, arg2) rdd_bw_cct_estim(rd_dat, method=arg1, kernel=arg2)$bws, 
       arg1 = rdd_bw_cct_estim_ARG_1,
       arg2 = rdd_bw_cct_estim_ARG_2)

## CCT plot
rdd_bw_cct_plot_ARG_1 <-  c("esmv", "es", "espr", "esmvpr", 
                             "qs", "qspr", "qsmv", "qsmvpr")
mapply(function(arg1) rdd_bw_cct_plot(rd_dat, method=arg1), 
       arg1 = rdd_bw_cct_plot_ARG_1, SIMPLIFY=FALSE)

## rsw 
rdd_bw_rsw_ARG_1 <- c("global", "sided")
mapply(function(arg1) rdd_bw_rsw(rd_dat, type=arg1), 
       arg1 = rdd_bw_rsw_ARG_1)


