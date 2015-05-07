
library(rddtools)
library(rdd)

## simple MC:
set.seed(123)

MC_simple <- function(n=200, CATE=0.3, HATE=0.1){
  x <- rnorm(n, mean=20, sd=5)
  D <- x>= 20
  y <- 0.8 + CATE*D+ 0.3*x+HATE*x*D+rnorm(n, sd=0.1)
  cat("effect", CATE+HATE*20, "\n")
  rdd_data(x=x, y=y, cutpoint=20)

}

input_mc <- MC_simple(n=1000, CATE=0.4)
plot(input_mc)

RDD_bw <- RDDbw_IK(input_mc)

RDD_np_sep <- RDDreg_np(input_mc, bw=RDD_bw)
RDD_np_same <- RDDreg_np(input_mc, slope="same", bw=RDD_bw)
RDD_np_sep_inflm <- RDDreg_np(input_mc, bw=RDD_bw, inf="lm")
RDD_np_same_inflm <- RDDreg_np(input_mc, slope="same", bw=RDD_bw, inf="lm")
RDD_lm_sep <- RDDreg_lm(input_mc, bw=RDD_bw)
RDD_lm_same <- RDDreg_lm(input_mc, slope="same", bw=RDD_bw)
rdd_RDe <- RDestimate(y~x, data=input_mc, cutpoint=20, model=TRUE, bw=RDD_bw)


printCoefmat(coef(summary(RDD_np_sep_inflm$RDDslot$model)))
printCoefmat(coef(summary(RDD_np_same_inflm$RDDslot$model)))
printCoefmat(coef(summary(RDD_lm_sep)))
printCoefmat(coef(summary(RDD_lm_same)))
printCoefmat(coef(summary(rdd_RDe $model[[1]])))


## few checks:
plse <- plotSensi(RDD_np_sep, from=5, to=20, by=0.5)
plotPlacebo(RDD_np_sep)

plotSensi(RDD_np_same, from=5, to=20, by=0.5)
plotPlacebo(RDD_np_same)

a<-plotSensi(RDD_lm_sep, from=5, to=20, by=0.5)
plotPlacebo(RDD_lm_sep)

plotSensi(RDD_lm_same, from=5, to=20, by=0.5)
plotPlacebo(RDD_lm_same)

#### Other MCs:
set.seed(123)
head(gen_MC_IK())

set.seed(123)
head(gen_MC_IK(output="rdd_data"))

set.seed(123)
head(gen_MC_IK(version=2))

set.seed(123)
head(gen_MC_IK(version=3))

set.seed(123)
head(gen_MC_IK(version=4))
