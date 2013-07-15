library(RDDtools)




############################################
### STEP 0: Data Manipulation
############################################
data(Lee2008)
head(Lee2008)

Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

head(Lee2008_rdd)

summary(Lee2008_rdd)

## With covariates

n_Lee <- nrow(Lee2008)

set.seed(123)
Z<- data.frame(z1=rnorm(n_Lee), z2=rnorm(n_Lee, mean=20, sd=2), z3=sample(letters[1:3], size=n_Lee, replace=TRUE))
Lee2008_rdd_z <- RDDdata(y=Lee2008$y, x=Lee2008$x, z=Z,cutpoint=0)

head(Lee2008_rdd_z )
summary(Lee2008_rdd_z )




############################################
### STEP 2: Graphical inspection
############################################

### Plot 
plot(Lee2008_rdd)
plot(Lee2008_rdd, nplot=3, h=c(0.02, 0.03, 0.04))
plot(Lee2008_rdd, nplot=1, h=0.1)

plot(Lee2008_rdd, xlim=c(-0.5, 0.5))

# plot(Lee2008_rdd, xlim=c(-0.5, 0.5), type="ggplot")


############################################
### STEP 2: Regression
############################################

## few bandwidths:
RDDbw_RSW(Lee2008_rdd)
RDDbw_IK(Lee2008_rdd)


###### Parametric regression ######
# Simple polynomial of order 1:
reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
print(reg_para)
summary(reg_para)
plot(reg_para)


## Difference in means regression:
# Simple polynomial of order 0:
reg_para_0 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=0)
print(reg_para_0)
summary(reg_para_0)
plot(reg_para_0)


## Simple polynomial of order 4:
reg_para4 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=4)
reg_para4
plot(reg_para4)

## Restrict sample to bandwidth area:
bw_ik <- RDDbw_IK(Lee2008_rdd)
reg_para_ik <- RDDreg_lm(RDDobject=Lee2008_rdd, bw=bw_ik, order=4)
reg_para_ik
plot(reg_para_ik)

## Covariates:
reg_para4_cov <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates=".")
reg_para4_cov
summary(reg_para4_cov)

reg_para4_cov_startR <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates=".", covar.opt=list(strategy="residual"))	
reg_para4_cov_startR
summary(reg_para4_cov_startR)

plot(reg_para4_cov)

reg_para4_cov_2 <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates="z3+I(z1^2)")
reg_para4_cov_2
summary(reg_para4_cov_2)

###### Non-parametric regression ######
reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
print(reg_nonpara)
summary(reg_nonpara)
plot(x=reg_nonpara)

reg_nonpara_inflm <- RDDreg_np(RDDobject=Lee2008_rdd, inference="lm")
print(reg_nonpara_inflm)
summary(reg_nonpara_inflm)
plot(x=reg_nonpara_inflm)


reg_nonpara_sameSl <- RDDreg_np(RDDobject=Lee2008_rdd, slope="same")
print(reg_nonpara_sameSl)
summary(reg_nonpara_sameSl)


###### PLOT SENSI ######
plSe_reg_para <- plotSensi(reg_para_ik, order=4:6)
plSe_reg_para_fac <- plotSensi(reg_para_ik, type="facet", order=4:6)
plSe_reg_para
plSe_reg_para_fac


plSe_reg_nonpara <- plotSensi(reg_nonpara)
plSe_reg_nonpara

plSe_reg_nonpara_HC <- plotSensi(reg_nonpara_inflm, vcov. =function(x) vcovCluster(x, clusterVar=model.frame(x)$x))
plSe_reg_nonpara_HC

plSe_reg_para_0 <- plotSensi(reg_para_0)
plSe_reg_para_0


###### Post-inference: ######

clusterInf(reg_para, clusterVar=model.frame(reg_para)$x, type="df-adj")
clusterInf(reg_para, clusterVar=model.frame(reg_para)[, "x^1"], type="HC")


############################################
### STEP 3: Validty tests
############################################

## Placebo test:
placeb_dat_reg_nonpara <- computePlacebo(reg_nonpara)

plotPlacebo(placeb_dat_reg_nonpara)
plotPlacebo(placeb_dat_reg_nonpara, device="base")


plotPlaceboDens(placeb_dat_reg_nonpara)

ptPl_reg_nonpara <- plotPlacebo(reg_nonpara)
ptPl_reg_nonpara

# with HC:
ptPl_reg_nonpara_HC <- plotPlacebo(reg_nonpara_inflm, vcov. =function(x) vcovCluster(x, clusterVar=model.frame(x)$x))
ptPl_reg_nonpara_HC

ptPl_reg_para_0 <- plotPlacebo(reg_para_0)
ptPl_reg_para_0



## density tests
dens_test(Lee2008_rdd)

## Covariates tests
covarTest_mean(Lee2008_rdd_z)
covarTest_mean(Lee2008_rdd_z, bw=0.1)
covarTest_dis(Lee2008_rdd_z)
covarTest_dis(Lee2008_rdd_z, bw=0.1)

#### as npreg
  reg_nonpara_np <- as.npreg(reg_nonpara, adjustIK_bw=FALSE)
  reg_nonpara_np
  RDDcoef(reg_nonpara_np)
  RDDcoef(reg_nonpara_np, allCo=TRUE)
  RDDcoef(reg_nonpara_np, allInfo=TRUE)
  RDDcoef(reg_nonpara_np, allInfo=TRUE, allCo=TRUE)

## Compare with result obtained with a Gaussian kernel:
  bw_lm <- dnorm(Lee2008_rdd$x, sd=RDDtools:::getBW(reg_nonpara))
  reg_nonpara_gaus <- RDDreg_lm(RDDobject=Lee2008_rdd, w=bw_lm)
  all.equal(RDDcoef(reg_nonpara_gaus, allCo=TRUE),RDDcoef(reg_nonpara_np, allCo=TRUE), check.attributes=FALSE) 
