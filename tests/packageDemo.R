library(rddtools)




############################################ STEP 0: Data Manipulation
data(house)
head(house)

house_rdd <- rdd_data(y = house$y, x = house$x, cutpoint = 0)

head(house_rdd)

summary(house_rdd)

## With covariates

n_Lee <- nrow(house)

set.seed(123)
Z <- data.frame(z1 = rnorm(n_Lee), z2 = rnorm(n_Lee, mean = 20, sd = 2), z3 = sample(letters[1:3], size = n_Lee, replace = TRUE))
house_rdd_z <- rdd_data(y = house$y, x = house$x, covar = Z, cutpoint = 0)

head(house_rdd_z)
summary(house_rdd_z)

### Fuzzy
set.seed(123)
ins <- rbinom(n_Lee, 1, prob = ifelse(house$x < 0, 0.1, 0.9))
house_rdd_ins <- rdd_data(y = house$y, x = house$x, z = ins, cutpoint = 0)
table(house$x < 0, ins == 0)

############################################ STEP 2: Graphical inspection

### Plot
plot(house_rdd)
plot(house_rdd, nplot = 3, h = c(0.02, 0.03, 0.04))
plot(house_rdd, nplot = 1, h = 0.1)

plot(house_rdd, xlim = c(-0.5, 0.5))

# plot(house_rdd, xlim=c(-0.5, 0.5), type='ggplot')


############################################ STEP 2: Regression

## few bandwidths:
rdd_bw_rsw(house_rdd)
rdd_bw_ik(house_rdd)


###### Parametric regression ###### Simple polynomial of order 1:
reg_para <- rdd_reg_lm(rdd_object = house_rdd)
print(reg_para)
summary(reg_para)
plot(reg_para)

all.equal(unlist(rdd_pred(reg_para)), rdd_coef(reg_para, allInfo = TRUE)[1:2], check.attributes = FALSE)

## Difference in means regression: Simple polynomial of order 0:
reg_para_0 <- rdd_reg_lm(rdd_object = house_rdd, order = 0)
print(reg_para_0)
summary(reg_para_0)
plot(reg_para_0)


## Simple polynomial of order 4:
reg_para4 <- rdd_reg_lm(rdd_object = house_rdd, order = 4)
reg_para4
plot(reg_para4)
all.equal(unlist(rdd_pred(reg_para4)), rdd_coef(reg_para4, allInfo = TRUE)[1:2], check.attributes = FALSE)

## Restrict sample to bandwidth area:
bw_ik <- rdd_bw_ik(house_rdd)
reg_para_ik <- rdd_reg_lm(rdd_object = house_rdd, bw = bw_ik, order = 4)
reg_para_ik
plot(reg_para_ik)

all.equal(unlist(rdd_pred(reg_para_ik)), rdd_coef(reg_para_ik, allInfo = TRUE)[1:2], check.attributes = FALSE)

## Fuzzy reg
reg_para_fuzz <- rdd_reg_lm(house_rdd_ins)
coef(reg_para_fuzz)
summary(reg_para_fuzz)

## Covariates:
reg_para4_cov <- rdd_reg_lm(rdd_object = house_rdd_z, order = 4, covariates = ".")
reg_para4_cov
summary(reg_para4_cov)

reg_para4_cov_slSep <- rdd_reg_lm(rdd_object = house_rdd_z, order = 4, covariates = ".", covar.opt = list(slope = "separate"))
summary(reg_para4_cov_slSep)
rdd_pred(reg_para4_cov_slSep)
rdd_pred(reg_para4_cov_slSep, covdata = data.frame(z1 = c(0, 0.2, 0.2), z2 = c(0, 20, 20), z3b = c(0, 1, 0), z3c = c(0, 0, 1)))


reg_para4_cov_startR <- rdd_reg_lm(rdd_object = house_rdd_z, order = 4, covariates = ".", covar.opt = list(strategy = "residual"))
reg_para4_cov_startR
summary(reg_para4_cov_startR)

plot(reg_para4_cov)

reg_para4_cov_startR_sl2 <- rdd_reg_lm(rdd_object = house_rdd_z, order = 4, covariates = ".", covar.opt = list(strategy = "residual", 
    slope = "separate"))
summary(reg_para4_cov_startR_sl2)

reg_para4_cov_2 <- rdd_reg_lm(rdd_object = house_rdd_z, order = 4, covariates = "z3+I(z1^2)")
reg_para4_cov_2
summary(reg_para4_cov_2)

###### Non-parametric regression ######
reg_nonpara <- rdd_reg_np(rdd_object = house_rdd)
print(reg_nonpara)
summary(reg_nonpara)
plot(x = reg_nonpara)

reg_nonpara_inflm <- rdd_reg_np(rdd_object = house_rdd, inference = "lm")
print(reg_nonpara_inflm)
summary(reg_nonpara_inflm)
plot(x = reg_nonpara_inflm)


reg_nonpara_sameSl <- rdd_reg_np(rdd_object = house_rdd, slope = "same")
print(reg_nonpara_sameSl)
summary(reg_nonpara_sameSl)


###### PLOT SENSI ######
plSe_reg_para <- plotSensi(reg_para_ik, order = 4:6)
plSe_reg_para_fac <- plotSensi(reg_para_ik, type = "facet", order = 4:6)
plSe_reg_para
plSe_reg_para_fac


plSe_reg_nonpara <- plotSensi(reg_nonpara)
plSe_reg_nonpara

plSe_reg_nonpara_HC <- plotSensi(reg_nonpara_inflm, vcov. = function(x) vcovCluster(x, clusterVar = model.frame(x)$x))
plSe_reg_nonpara_HC

plSe_reg_para_0 <- plotSensi(reg_para_0, plot = FALSE)
plSe_reg_para_0

plSe_reg_para_0_gg <- plotSensi(reg_para_0, plot = FALSE, output = "ggplot")
str(plSe_reg_para_0_gg)


###### Post-inference: ######

clusterInf(reg_para, clusterVar = model.frame(reg_para)$x, type = "df-adj")
clusterInf(reg_para, clusterVar = model.frame(reg_para)$x, type = "HC")


############################################ STEP 3: Validty tests

## Placebo test:
placeb_dat_reg_nonpara <- computePlacebo(reg_nonpara)

plotPlacebo(placeb_dat_reg_nonpara)
plotPlacebo(placeb_dat_reg_nonpara, device = "base")


plotPlaceboDens(placeb_dat_reg_nonpara)

## check invisible return:
ptPl_reg_nonpara <- plotPlacebo(reg_nonpara, plot = FALSE)
ptPl_reg_nonpara

ptPl_reg_nonpara2 <- plotPlacebo(reg_nonpara, plot = FALSE, output = "ggplot")
ptPl_reg_nonpara2

# with HC:
ptPl_reg_nonpara_HC <- plotPlacebo(reg_nonpara_inflm, vcov. = function(x) vcovCluster(x, clusterVar = model.frame(x)$x))
ptPl_reg_nonpara_HC

ptPl_reg_para_0 <- plotPlacebo(reg_para_0)
ptPl_reg_para_0



## density tests
dens_test(house_rdd)
dens_test(reg_para_0, plot = FALSE)
dens_test(reg_nonpara, plot = FALSE)$test.output[c("theta", "se", "z", "p", "binsize", "bw", "cutpoint")]


## Covariates tests
covarTest_mean(house_rdd_z)
covarTest_mean(house_rdd_z, bw = 0.1)
covarTest_dis(house_rdd_z)
covarTest_dis(house_rdd_z, bw = 0.1)

covarTest_mean(reg_para4_cov)
covarTest_dis(reg_para4_cov)
#### as npreg
reg_nonpara_np <- as.npreg(reg_nonpara, adjustik_bw = FALSE)
reg_nonpara_np
rdd_coef(reg_nonpara_np)
rdd_coef(reg_nonpara_np, allCo = TRUE)
rdd_coef(reg_nonpara_np, allInfo = TRUE)
rdd_coef(reg_nonpara_np, allInfo = TRUE, allCo = TRUE)

## Compare with result obtained with a Gaussian kernel:
bw_lm <- dnorm(house_rdd$x, sd = rddtools:::getBW(reg_nonpara))
reg_nonpara_gaus <- rdd_reg_lm(rdd_object = house_rdd, w = bw_lm)
all.equal(rdd_coef(reg_nonpara_gaus, allCo = TRUE), rdd_coef(reg_nonpara_np, allCo = TRUE), check.attributes = FALSE)



#### methods

regs_all <- list(reg_para = reg_para, reg_para_0 = reg_para_0, reg_para4 = reg_para4, reg_para_ik = reg_para_ik, reg_para_fuzz = reg_para_fuzz, 
    reg_para4_cov = reg_para4_cov, reg_para4_cov_slSep = reg_para4_cov_slSep, reg_para4_cov_startR = reg_para4_cov_startR, reg_para4_cov_startR_sl2 = reg_para4_cov_startR_sl2, 
    reg_nonpara = reg_nonpara, reg_nonpara_inflm = reg_nonpara_inflm, reg_nonpara_sameSl = reg_nonpara_sameSl)
capply <- function(x) {
    n.obs <- sapply(x, length)
    seq.max <- seq_len(max(n.obs))
    t(sapply(x, "[", i = seq.max))
}

capply(lapply(regs_all, coef))
sapply(regs_all, rdd_coef)
rdd_pred_issue <- c("reg_para_0", "reg_para_fuzz", "reg_nonpara", "reg_nonpara_sameSl")
sapply(regs_all[!names(regs_all) %in% rdd_pred_issue], rdd_pred)

sapply(regs_all, rddtools:::getCutpoint)
lapply(regs_all, plotSensi, plot = FALSE)

sapply(regs_all, function(x) dens_test(x, plot = FALSE)[c("p.value", "statistic", "estimate")])
 
