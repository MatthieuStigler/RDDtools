
library(rdd)
library(RDDtools)

set.seed(1234)
x<-runif(1000,-1,1)
cov<-rnorm(1000)
y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)

RD <- RDDdata(x=x, y=y, cutpoint=0, z=cov)

### Simple estimation:
bw <- IKbandwidth(X=x, Y=y, cutpoint=0)
rdd_mod <- RDestimate(y~x, bw=bw, se.type="const", model=TRUE)$model[[1]]
RDDtools_mod <- RDDreg_np(RD, bw=bw, inference="lm")

rdd_co <- coef(summary(rdd_mod))
RDDtools_co <- RDDcoef(RDDtools_mod, allCo=TRUE, allInfo=TRUE)
rdd_co
RDDtools_co

all.equal(rdd_co[-4,], RDDtools_co[c(1,3,2),], check=FALSE)


### Covariate estimation:
rdd_mod_cov <- RDestimate(y~x|cov, kernel="rectangular", bw=5, model=TRUE, se.type="const")$model[[1]]
RDDtools_mod_cov <- RDDreg_lm(RD, bw=5, covariates="cov", covar.opt=list(slope="separate"))

rdd_co_cov <- coef(summary(rdd_mod_cov))
RDDtools_co_cov <- RDDcoef(RDDtools_mod_cov, allCo=TRUE, allInfo=TRUE)
rdd_co_cov
RDDtools_co_cov

all.equal(rdd_co_cov[-4,], RDDtools_co_cov[-4,], check=FALSE)

