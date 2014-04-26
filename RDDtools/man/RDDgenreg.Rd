\name{RDDgenreg}
\alias{RDDgenreg}
\title{General polynomial estimator of the regression discontinuity}
\usage{
RDDgenreg(RDDobject, fun = glm, covariates = NULL, order = 1, bw = NULL,
  slope = c("separate", "same"), covar.opt = list(strategy = c("include",
  "residual"), slope = c("same", "separate"), bw = NULL), weights, ...)
}
\arguments{
  \item{RDDobject}{Object of class RDDdata created by
  \code{\link{RDDdata}}}

  \item{covariates}{Formula to include covariates}

  \item{order}{Order of the polynomial regression.}

  \item{bw}{A bandwidth to specify the subset on which the
  kernel weighted regression is estimated}

  \item{weights}{Optional weights to pass to the lm
  function. Note this cannot be entered together with
  \code{bw}}

  \item{slope}{Whether slopes should be different on left
  or right (separate), or the same.}

  \item{covar.opt}{Options for the inclusion of covariates.
  Way to include covariates, either in the main regression
  (\code{include}) or as regressors of y in a first step
  (\code{residual}).}

  \item{fun}{The function to estimate the parameters}

  \item{\ldots}{Further arguments passed to fun. See the
  example.}
}
\value{
An object of class RDDreg_lm and class lm, with specific
print and plot methods
}
\description{
Compute RDD estimate allowing a locally kernel weighted
version of any estimation function possibly on the range
specified by bandwidth
}
\details{
This function allows the user to use a custom estimating
function, instead of the traditional \code{lm()}. It is
assumed that the custom funciton has following behaviour:
\enumerate{ \item A formula interface, together with a
\code{data} argument \item A \code{weight} argument \item A
coef(summary(x)) returning a data-frame containing a column
Estimate } Note that for the last requirement, this can be
accomodated by writing a specific \code{\link{RDDcoef}}
function for the class of the object returned by
\code{fun}.
}
\examples{
## Step 0: prepare data
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)

## Estimate a local probit:
Lee2008_rdd$y <- with(Lee2008_rdd, ifelse(y<quantile(y, 0.25), 0,1))
reg_bin_glm <- RDDgenreg(RDDobject=Lee2008_rdd, fun= glm, family=binomial(link="probit"))
print(reg_bin_glm)
summary(reg_bin_glm)
}
\references{
TODO
}

