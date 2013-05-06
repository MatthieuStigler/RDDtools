\name{dens_test}
\alias{dens_test}
\title{Run the McCracy test for manipulation of the forcing variable}
\usage{
  dens_test(RDDobject, bin = NULL, bw = NULL)
}
\arguments{
  \item{RDDobject}{object of class RDDdata}

  \item{bin}{Argument of the \code{\link{DCdensity}}
  function, the binwidth}

  \item{bw}{Argument of the \code{\link{DCdensity}}
  function, the bandwidth}
}
\description{
  Calls the \code{\link{DCdensity}} test from package rdd
  on a \code{RDDobject}
}
\examples{
library(RDDtools)
data(Lee2008)
Lee2008_rdd <- RDDdata(y=Lee2008$y, x=Lee2008$x, cutpoint=0)
dens_test(Lee2008_rdd)
}

