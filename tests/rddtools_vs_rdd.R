
library(rdd)
library(rddtools)

set.seed(1234)
x<-runif(1000,-1,1)
cov<-rnorm(1000)
y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)

RD <- rdd_data(x=x, y=y, cutpoint=0, covar=cov)

### Simple estimation:
bw <- IKbandwidth(X=x, Y=y, cutpoint=0)
bw
rdd_mod <- RDestimate(y~x, bw=bw, se.type="const", model=TRUE)$model[[1]]
RDDtools_mod <- rdd_reg_np(RD, bw=bw, inference="lm")

rdd_co <- coef(summary(rdd_mod))
RDDtools_co <- rdd_coef(RDDtools_mod, allCo=TRUE, allInfo=TRUE)
rdd_co
RDDtools_co

all.equal(rdd_co[-4,], RDDtools_co[1:3,], check.attributes=FALSE)
all.equal(rdd_co[4,1], sum(RDDtools_co[3:4,1]), check.attributes=FALSE)


### Covariate estimation:
rdd_mod_cov <- RDestimate(y~x|cov, kernel="rectangular", bw=5, model=TRUE, se.type="const")$model[[1]]
RDDtools_mod_cov <- rdd_reg_lm(RD, bw=5, covariates="cov", covar.opt=list(slope="separate"))

rdd_co_cov <- coef(summary(rdd_mod_cov))
RDDtools_co_cov <- rdd_coef(RDDtools_mod_cov, allCo=TRUE, allInfo=TRUE)
rdd_co_cov
RDDtools_co_cov

all.equal(rdd_co_cov[-4,], RDDtools_co_cov[-4,], check.attributes=FALSE)

## Fuzzy
set.seed(123)
selec <- rbinom(nrow(RD), 1, prob=ifelse(RD$x<0, 0.1, 0.9))
RD_rdd_ins <- rdd_data(y=RD$y, x=RD$x, z=selec,cutpoint=0)

RDDto_reg_fuz <- rdd_reg_lm(RD_rdd_ins, bw=0.2)
rdd_reg_fuz <- RDestimate(y~x+selec, data=RD_rdd_ins, kernel="rectangular", bw=0.2, model=TRUE, se.type="const")$model[[2]][[1]]

all.equal(rdd_coef(RDDto_reg_fuz),coef(summary(rdd_reg_fuz))[2,1])
all.equal(rdd_coef(RDDto_reg_fuz, allCo=TRUE)[1:3],coef(summary(rdd_reg_fuz))[1:3,1], check.attributes=FALSE)

