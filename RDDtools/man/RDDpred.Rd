\name{RDDpred}
\alias{RDDpred}
\title{RDD coefficient prediction}
\usage{
  RDDpred(object, covdata, se.fit = TRUE, vcov. = NULL)
}
\arguments{
  \item{object}{A RDD regression object}

  \item{covdata}{New data.frame specifying the values of
  the covariates, can have multiple rows.}

  \item{se.fit}{A switch indicating if standard errors are
  required.}

  \item{vcov.}{Specific covariance function (see package
  sandwich ), by default uses the \code{\link{vcov}}}
}
\value{
  Returns the predicted value(s), and, if se.fit=TRUE,
  their standard errors.
}
\description{
  Function to predict the RDD coefficient in presence of
  covariate (without covariates, returns the same than
  \code{\link{RDDcoef}})
}
\details{
  The function RDDpred does a simple prediction of the RDD
  effect \deqn{RDDeffect= \mu(x, z, D=1) - \mu(x, z, D=0)}
  When there are no covariates (and z is irrelevant in the
  equation above), this amounts exactly to the usual RDD
  coefficient, shown in the outputs, or obtained with
  \code{\link{RDDcoef}}. If there were covariates, and if
  these covariates were added using the \dQuote{include}
  \emph{strategy}, and that furthermore they were estimated
  with different coefficients left and right to the cutoff
  (i.e. had argument \emph{slope} = \dQuote{separate}),
  than the RDD effect is also dependent on the value of the
  covariate. RDDpred allows to set the value of the
  covariate(s) at which to evaluate the RDD effect, by
  providing a data.frame with the values for the
  covariates. Note that the effect can be evaluated at
  multiple points, if you provide multiple rows of
  \code{covdata}.
}
\examples{
## Load data, add (artificial) covariates:
  data(Lee2008)
  Lee2008_rdd <- RDDdata(y=y, x=x, data=Lee2008, z=runif(n_Lee), cutpoint=0)

## estimation without covariates: RDDpred is the same than RDDcoef:
  reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)

  RDDpred(reg_para)
  RDDcoef(reg_para, allInfo=TRUE)

## estimation with covariates:
  reg_para_cov <- RDDreg_lm(RDDobject=Lee2008_rdd_z, covariates="z1", covar.opt=list(slope="separate"))
  RDDpred(reg_para_cov, covdata=data.frame(z1=0)) ## should obtain same result than with RDestimate
  RDDpred(reg_para_cov, covdata=data.frame(z1=0.5)) #evaluate at mean of z1 (as comes from uniform)
}

