\name{covarTest_mean}
\alias{covarTest_mean}
\title{Testing for balanced covariates: equality of means with t-test}
\usage{
covarTest_mean(object, bw = NULL, paired = FALSE, var.equal = FALSE,
  p.adjust = c("none", "holm", "BH", "BY", "hochberg", "hommel",
  "bonferroni"))
}
\arguments{
  \item{object}{object of class RDDdata}

  \item{bw}{a bandwidth}

  \item{paired}{Argument of the \code{\link{t.test}}
  function: logical indicating whether you want paired
  t-tests.}

  \item{var.equal}{Argument of the \code{\link{t.test}}
  function: logical variable indicating whether to treat
  the two variances as being equal}

  \item{p.adjust}{Whether to adjust the p-values for
  multiple testing. Uses the \code{\link{p.adjust}}
  function}

  \item{\ldots}{currently not used}
}
\value{
A data frame wih
}
\description{
Tests equality of means by a t-test for each covariate,
between the two full groups or around the discontinuity
threshold
}
\author{
Matthieu Stigler <\email{Matthieu.Stigler@gmail.com}>
}

