\name{covarTest_dis}
\alias{covarTest_dis}
\title{Testing for balanced covariates: equality of distribution}
\usage{
  covarTest_dis(object, bw, exact = NULL,
    p.adjust = c("none", "holm", "BH", "BY", "hochberg", "hommel", "bonferroni"))
}
\arguments{
  \item{object}{object of class RDDdata}

  \item{h}{a bandwidth}

  \item{exact}{Argument of the \code{\link{ks.test}}
  function: NULL or a logical indicating whether an exact
  p-value should be computed.}

  \item{p.adjust}{Whether to adjust the p-values for
  multiple testing. Uses the \code{\link{p.adjust}}
  function}

  \item{\ldots}{currently not used}
}
\value{
  A data frame wih
}
\description{
  Tests equality of distribution with a Kolmogorov-Smirnov
  for each covariates, between the two full groups or
  around the discontinuity threshold
}
\author{
  Matthieu Stigler <\email{Matthieu.Stigler@gmail.com}>
}

