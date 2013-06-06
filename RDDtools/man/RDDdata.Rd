\name{RDDdata}
\alias{RDDdata}
\title{Construct RDDdata}
\usage{
  RDDdata(y, x, z, cutpoint, labels)
}
\arguments{
  \item{x}{Forcing variable}

  \item{y}{Output}

  \item{z}{Exogeneous variables}

  \item{cutpoint}{Cutpoint}

  \item{labels}{Additional labels to provide as list (with
  entries x, y, and eventually vector z)}
}
\value{
  Object of class \code{RDDdata}, inheriting from
  \code{data.frame}
}
\description{
  Construct the base RDD object, containing x, y and the
  cutpoint, eventuallay covariates.
}
\examples{
data(Lee2008)
rd<- RDDdata(x=Lee2008$x, y=Lee2008$y, cutpoint=0)
rd
summary(rd)
}
\author{
  Matthieu Stigler <\email{Matthieu.Stigler@gmail.com}>
}

