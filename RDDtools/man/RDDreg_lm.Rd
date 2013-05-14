\name{RDDreg_lm}
\alias{RDDreg_lm}
\title{Parametric polynomial estimator of the regression discontinuity}
\usage{
  RDDreg_lm(RDDobject, covariates = NULL, order = 1,
    bw = NULL, slope = c("separate", "same"),
    covar.strat = c("include", "residual"), weights)
}
\arguments{
  \item{RDDobject}{Object of class RDDdata created by
  \code{\link{RDDdata}}}

  \item{covariates}{TODO}

  \item{order}{Order of the polynomial regression.}

  \item{bw}{A bandwidth to specify the subset on which the
  parametric regression is estimated}

  \item{weights}{Optional weights to pass to the lm
  function. Note this cannot be entered together with
  \code{bw}}

  \item{slope}{Whether slopes should be different on left
  or right (separate), or the same.}
}
\value{
  An object of class RDDreg_lm and class lm, with specific
  print and plot methods
}
\description{
  Compute a parametric polynomial regression of the ATE,
  possibly on the range specified by bandwidth
}
\examples{
## Step 0: prepare data
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
## Step 2: regression
# Simple polynomial of order 1:
reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)
print(reg_para)
plot(reg_para)

# Simple polynomial of order 4:
reg_para4 <- RDDreg_lm(RDDobject=Lee2008_rdd, order=4)
reg_para4
plot(reg_para4)

# Restrict sample to bandwidth area:
bw_ik <- RDDbw_IK(Lee2008_rdd)
reg_para_ik <- RDDreg_lm(RDDobject=Lee2008_rdd, bw=bw_ik, order=4)
reg_para_ik
plot(reg_para_ik)
}
\references{
  TODO
}

