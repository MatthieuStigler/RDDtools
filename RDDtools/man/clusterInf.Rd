\name{clusterInf}
\alias{clusterInf}
\title{Post-inference for clustered data}
\usage{
  clusterInf(object, clusterVar, vcov. = NULL,
    type = c("df-adj", "HC"), ...)
}
\arguments{
  \item{object}{Object of class lm, from which RDDreg also
  inherits.}

  \item{clusterVar}{The variable containing the cluster
  attributions.}

  \item{vcov.}{Specific covariance function to pass to
  coeftest. See help of sandwich}

  \item{type}{The type of cluster correctin to do: either
  the degrees of freedom, or a HC matrix.}

  \item{\ldots}{Further argumetns passed to coeftest}
}
\value{
  The output of the coeftest function, which is itself of
  class \code{coeftest}
}
\description{
  Correct standard-errors to account for clustered data,
  doing either a degrees of freedom correction or using a
  heteroskedasticidty-cluster robust covariance matrix
  possibly on the range specified by bandwidth
}
\examples{
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
reg_para <- RDDreg_lm(RDDobject=Lee2008_rdd)

# here we just generate randomly a cluster variable:
nlet <- sort(c(outer(letters, letters, paste, sep="")))
clusRandom <- sample(nlet[1:60], size=nrow(Lee2008_rdd), replace=TRUE)

# now do post-inference:
clusterInf(reg_para, clusterVar=clusRandom)
clusterInf(reg_para, clusterVar=clusRandom, type="HC")
}

