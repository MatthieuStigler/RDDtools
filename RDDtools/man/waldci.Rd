\name{waldci}
\alias{waldci}
\title{Confint allowing vcov}
\usage{
  waldci(x, parm = NULL, level = 0.95, vcov. = NULL,
    df = NULL, ...)
}
\arguments{
  \item{x}{Object of class lm or else}

  \item{parm}{specification of which parameters are to be
  given confidence intervals, see confint}

  \item{level}{the confidence level required, see
  confint()}

  \item{vcov.}{Specific covariance function to pass to
  coeftest. See help of sandwich}

  \item{df}{Degrees of freedom}

  \item{\ldots}{Further argument}
}
\description{
  Version of vcov allowing for confint
}
\keyword{internal}

