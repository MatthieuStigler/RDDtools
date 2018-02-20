library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
set.seed(123)
cov_df <- data.frame(cov=rnorm(nrow(house)))
set.seed(123)
z_fake <- ifelse(house$x>0+rnorm(nrow(house), sd=0.05),1,0)
rd_dat <- rdd_data(x=x, y=y, data=house, cutpoint=0)
rd_dat_fakefuzzy <- rdd_data(x=house$x, y=house$y, z=ifelse(house$x>0,1,0), cutpoint=0)
rd_dat_fuzzy <- rdd_data(x=house$x, y=house$y, z=z_fake, 
                         cutpoint=0, covar=cov_df)
rd_dat_covar <- rdd_data(x=house$x, y=house$y, covar=cov_df, cutpoint=0)

summary(rd_dat)
summary(rd_dat_fakefuzzy)
summary(rd_dat_fuzzy)
summary(rd_dat_covar)

reg_lm <- rdd_reg_lm(rd_dat)
reg_lm_covar <- rdd_reg_lm(rd_dat_covar, covariates="cov")
reg_lm_fak_fuz <- rdd_reg_lm(rd_dat_fakefuzzy)
reg_lm_fuz <- rdd_reg_lm(rd_dat_fuzzy)
# reg_lm_fuz_cov <- rdd_reg_lm(rd_dat_fuzzy, covariates="cov")
reg_np <- rdd_reg_np(rd_dat)

reg_dat <- list(reg_lm=reg_lm, reg_np=reg_np,
                reg_lm_fak_fuz=reg_lm_fak_fuz, reg_lm_fuz=reg_lm_fuz,
                reg_lm_covar=reg_lm_covar)


res <- lapply(reg_dat, print)
lapply(reg_dat, summary)
lapply(reg_dat, coef)

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


