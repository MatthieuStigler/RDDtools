\name{RDDgenreg}
\alias{RDDgenreg}
\title{Parametric polynomial estimator of the regression discontinuity}
\usage{
  RDDgenreg(RDDobject, covariates = ".",
    bw = RDDbw_IK(RDDobject),
    slope = c("separate", "same"), fun = glm, ...)
}
\arguments{
  \item{RDDobject}{Object of class RDDdata created by
  \code{\link{RDDdata}}}

  \item{covariates}{TODO}

  \item{bw}{A bandwidth to specify the subset on which the
  parametric regression is estimated}

  \item{slope}{Whether slopes should be different on left
  or right (separate), or the same.}

  \item{fun}{Function estimating the model}

  \item{\ldots}{Furtehr arguments passed to fun}
}
\value{
  An object of class RDDreg_np and class lm, with specific
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
reg_nonpara <- RDDreg_np(RDDobject=Lee2008_rdd)
print(reg_nonpara)
plot(reg_nonpara)
}
\references{
  TODO
}

