library(rddtools)
library(car)


#### DATA
data(Lee2008)
Lee2008_rdd <- rdddata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

n_Lee <- nrow(Lee2008)

set.seed(123)
Z<- data.frame(z1=rnorm(n_Lee), z2=rnorm(n_Lee, mean=20, sd=2), z3=sample(letters[1:3], size=n_Lee, replace=TRUE))
Lee2008_rdd_z <- rdddata(y=Lee2008$y, x=Lee2008$x, covar=Z,cutpoint=0)

#### REGS
bw_IK <- RDDbw_IK(Lee2008_rdd_z)
w_IK <- RDDtools:::Kernel_tri(Lee2008_rdd_z$x, 0, bw_IK)
reg_para4_cov_slSep <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates="z1", covar.opt=list(slope="separate"))
reg_para4_cov_slSep_W <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates="z1", covar.opt=list(slope="separate"), weights=w_IK)
reg_np_cov <- RDDreg_np(RDDobject=Lee2008_rdd_z, covariates="z1", bw=bw_IK, inference="lm")




reg_para4_cov_slSep_2Z <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates="z1+z2", covar.opt=list(slope="separate"))

reg_li <- list( reg_para4_cov_slSep=reg_para4_cov_slSep, 
		reg_para4_cov_slSep_W=reg_para4_cov_slSep_W,
		reg_np_cov=reg_np_cov,
		reg_para4_cov_slSep_2Z=reg_para4_cov_slSep_2Z)

checkRDDmean <- function(x, n=5){
  covDF <- model.frame(x)
  zDF <- grep("z", colnames(covDF), value=FALSE)
  hasD <- zDF[-grep(":", colnames(covDF)[zDF])]

  DF_1 <- covDF[1:n,hasD, drop=FALSE]
  DF_2 <- data.frame(t(colMeans(DF_1)))

  pred_1 <- rddpred(x, covdata=DF_1, stat="mean")
  pred_2 <- rddpred(x, covdata=DF_2)
  all.equal(pred_1, pred_2, check.attributes=FALSE)
}

sapply(reg_li, checkRDDmean)

sapply(reg_li, function(x) all.equal(unlist(rddpred(x)),rddcoef(x, allInfo=TRUE)[1,1:2], check.attributes=FALSE))


# 
# reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
# print(reg_para)
# summary(reg_para)
# plot(reg_para)
# 
# formula(reg_para)
# 
# update(as.formula("y ~ D + `x^1` + `x^1_right`"), reg_para)
# reg_para_l <- as.lm(reg_para)
# # update(reg_para_l, y ~ D + `x^1` + `x^1_right`)
# 
# mf <- model.frame(reg_para)
# 
# lm("y ~ D + `x^1` + `x^1_right`", mf)
# a<-lm("y ~ -1 + D +I(1-D) + `x^1` + `x^1_right`", mf)
# diff(coef(a)[2:1])
# coef(reg_para)
# 
# # deltaMethod(a, "I(1-D) - D", parameterNames=paste("a", 1:4, sep=""))
# deltaMethod(a, "a1 - a2", parameterNames=paste("a", 1:4, sep=""))
# coef(summary(reg_para))[2,]
# 
# reg_para4_cov_slSep <- RDDreg_lm(RDDobject=Lee2008_rdd_z, order=4, covariates="z1", covar.opt=list(slope="separate"))
# 
mf_2 <- model.frame(reg_para4_cov_slSep)
# formula(reg_para4_cov_slSep)
# 
aa <- lm("y ~ D + `x` + `x^2` + `x^3` + `x^4` + `x_right` + `x^2_right` + `x^3_right` + `x^4_right` + z1 + `z1:D`", data=mf_2)
aaa <- lm("y ~ -1+ D + I(1-D)+`x` + `x^2` + `x^3` + `x^4` + `x_right` + `x^2_right` + `x^3_right` + `x^4_right` + z1 + `z1:D`", data=mf_2)
# 
# diff(coef(aaa)[2:1])
# rddpred(reg_para4_cov_slSep)
# rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=0))
# 
# rddcoef(reg_para4_cov_slSep, allInfo=TRUE)

## compare rddpred and Delta at 1:
rdd_p_1 <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=1))
delta_1 <- deltaMethod(aaa, "a1 - a2 + a12", parameterNames=paste("a", 1:12, sep=""))
rdd_p_1
delta_1
all.equal(unlist(rdd_p_1), drop(as.matrix(delta_1[1:2])), check.attributes=FALSE)

## compare rddpred and Delta at 0:
rdd_p_0 <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=0))
rdd_c_0 <- rddcoef(reg_para4_cov_slSep, allInfo=TRUE)
delta_0 <- deltaMethod(aaa, "a1 - a2 ", parameterNames=paste("a", 1:12, sep=""))
rdd_p_0
rdd_c_0
delta_0
all.equal(unlist(rdd_p_0), drop(as.matrix(delta_0[1:2])), check.attributes=FALSE)
all.equal(unlist(rdd_p_0), drop(as.matrix(rdd_c_0[1:2])), check.attributes=FALSE)

## compare rddpred and Delta at 2 points:
rdd_p_01_AGG <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0.5)))
rdd_p_01_all <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0, 1)))
rdd_p_01_S <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0, 1)), stat="sum")
rdd_p_01_M <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0, 1)), stat="mean")

delta_01_S <- deltaMethod(aaa, "2*(a1 - a2) +1*a12", parameterNames=paste("a", 1:12, sep=""))
delta_01_M <- deltaMethod(aaa, "(2*(a1 - a2) +1*a12)/2", parameterNames=paste("a", 1:12, sep=""))
delta_01_S
delta_01_M

all(delta_01_S/2==delta_01_M)

## compare individuals (stat=ident)
all.equal(rdd_p_01_all$fit, c(delta_0[1,1], delta_1[1,1]))
all.equal(rdd_p_01_all$se.fit, c(delta_0[1,2], delta_1[1,2]))
c(rdd_p_01_M$fit/2, rdd_p_01_AGG$fit)

## compare sum (stat=sum)
all.equal(unlist(rdd_p_01_S), drop(as.matrix(delta_01_S[1:2])), check.attributes=FALSE)

## compare mean (stat=mean)
all.equal(unlist(rdd_p_01_M), drop(as.matrix(delta_01_M[1:2])), check.attributes=FALSE)
all.equal(rdd_p_01_M$fit, rdd_p_01_S$fit/2)
all.equal(rdd_p_01_M$fit, rdd_p_01_AGG$fit, check.attributes=FALSE)
all.equal(rdd_p_01_M$se.fit, rdd_p_01_AGG$se.fit, check.attributes=FALSE)

## compare rddpred and Delta at 5 first points:
ind_z_pos <- head(which(Lee2008_rdd_z$z1>0),5)

rdd_p_01_5z_S <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=Lee2008_rdd_z$z1[1:5]), stat="sum")
rdd_p_01_5z_Sb <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=sum(Lee2008_rdd_z$z1[1:5])), stat="sum")
rdd_p_01_5zPos_S <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=Lee2008_rdd_z$z1[ind_z_pos]), stat="sum")
rdd_p_01_5zPos_Sb <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=sum(Lee2008_rdd_z$z1[ind_z_pos])), stat="sum")
rdd_p_01_5z_M <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=Lee2008_rdd_z$z1[1:5]), stat="mean")
rdd_p_01_5z_Mb <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=mean(Lee2008_rdd_z$z1[1:5])), stat="mean")
rdd_p_01_ALLz_M <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=Lee2008_rdd_z$z1), stat="mean")
rdd_p_01_ALLz_Mb <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=mean(Lee2008_rdd_z$z1)), stat="mean")

del <- function(x, mean=FALSE) {
  n <- length(x)
  res <- paste(c(paste(n, "*(a1-a2) "), paste(x, "*a12", sep="")), collapse=" +")
  su <- sum(x)
  sig <- if(sign(su)==1) "+" else  NULL
  res <- paste(n, "*(a1-a2) ", sig, su, "*a12", sep="")
  if(mean) res <- paste("(", res, ")/", n, sep="")
  res
}

del(x=Lee2008_rdd_z$z1[1:5])
delta_01_5z_S <- deltaMethod(aaa, del(x=Lee2008_rdd_z$z1[1:5]), parameterNames=paste("a", 1:12, sep=""), func="RDD")
delta_01_5z_M <- deltaMethod(aaa, del(x=Lee2008_rdd_z$z1[1:5], mean=TRUE), parameterNames=paste("a", 1:12, sep=""), func="RDD")

all.equal(unlist(rdd_p_01_5z_S), drop(as.matrix(delta_01_5z_S[1:2])), check.attributes=FALSE)
all.equal(unlist(rdd_p_01_5z_Sb), drop(as.matrix(delta_01_5z_S[1:2])), check.attributes=FALSE)
all.equal(unlist(rdd_p_01_5z_M), drop(as.matrix(delta_01_5z_M[1:2])), check.attributes=FALSE)
all.equal(unlist(rdd_p_01_5z_Mb), drop(as.matrix(delta_01_5z_M[1:2])), check.attributes=FALSE)

## All z:
# all.equal(rdd_p_01_ALLz_M, rdd_p_01_ALLz_Mb, check.attributes=FALSE)

#### Weighted mean!!
w_5 <- c(0.1, 0.2, 0.4, 0.2, 0.1)
w <- c(0.4, 0.6)
rdd_p_01_Sid <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0.2,1)), stat="identity")
wm <- weighted.mean(rdd_p_01_Sid$fit , w=w)

delta_2z_w <- deltaMethod(aaa, "0.4*(a1 - a2) + 0.4*0.2*a12+0.6*(a1 - a2) + 0.6*a12", parameterNames=paste("a", 1:12, sep=""))
delta_2z_w2 <- deltaMethod(aaa, "1*(a1 - a2) + 0.4*0.2*a12 + 0.6*a12", parameterNames=paste("a", 1:12, sep=""))
delta_2z_w3 <- deltaMethod(aaa, "1*(a1 - a2) + a12*(0.4*0.2 + 0.6)", parameterNames=paste("a", 1:12, sep=""))
all(delta_2z_w==delta_2z_w2)
all.equal(delta_2z_w, delta_2z_w3, check.attributes=FALSE)
all.equal(delta_2z_w[1,1],wm)

rdd_p_01_W_S <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0.2,1)), stat="sum", weights=w)
rdd_p_01_W_M <- rddpred(reg_para4_cov_slSep, covdata=data.frame(z1=c(0.2,1)), stat="mean", weights=w)
all.equal(rdd_p_01_W_M$fit,wm)

all.equal(unlist(rdd_p_01_W_S), drop(as.matrix(delta_2z_w2[1:2])), check.attributes=FALSE)
all.equal(unlist(rdd_p_01_W_M), drop(as.matrix(delta_2z_w2[1:2])), check.attributes=FALSE)


###### 2 Z:
df_2Z_5z <- Lee2008_rdd_z[1:5, c("z1", "z2")]
df_2Z_5z_M <- data.frame(t(colMeans(df_2Z_5z)))
df_2Z_5z_Mw <- data.frame(t(apply(df_2Z_5z, 2, weighted.mean, w=w_5)))

rdd_p_sZ_5z_S <- rddpred(reg_para4_cov_slSep_2Z, covdata=df_2Z_5z, stat="sum")
rdd_p_sZ_5z_M <- rddpred(reg_para4_cov_slSep_2Z, covdata=df_2Z_5z, stat="mean")
rdd_p_sZ_5z_Mb <- rddpred(reg_para4_cov_slSep_2Z, covdata=df_2Z_5z_M, stat="sum")

rdd_p_sZ_5z_MW <- rddpred(reg_para4_cov_slSep_2Z, covdata=df_2Z_5z, stat="mean", weights=w_5)
rdd_p_sZ_5z_MWb <- rddpred(reg_para4_cov_slSep_2Z, covdata=df_2Z_5z_Mw, stat="sum")

all.equal(rdd_p_sZ_5z_M, rdd_p_sZ_5z_Mb, check.attributes=FALSE)
all.equal(rdd_p_sZ_5z_MW, rdd_p_sZ_5z_MWb, check.attributes=FALSE)
