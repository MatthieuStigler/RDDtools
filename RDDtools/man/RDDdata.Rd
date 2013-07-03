\name{RDDdata}
\alias{RDDdata}
\title{Construct RDDdata}
\usage{
  RDDdata(y, x, z, cutpoint, labels, data)
}
\arguments{
  \item{x}{Forcing variable}

  \item{y}{Output}

  \item{z}{Exogeneous variables}

  \item{cutpoint}{Cutpoint}

  \item{labels}{Additional labels to provide as list (with
  entries x, y, and eventually vector z). Unused
  currently.}

  \item{data}{A data-frame for the x and y variables. If
  this is provided, the column names can be entered
  directly for argumetn \code{x} and \code{y}}
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
rd2 <- RDDdata(x=x, y=y, data=Lee2008, cutpoint=0)

# The print() function is the same as the print.data.frame:
rd

# The summary() and plot() function are specific to RDDdata
summary(rd)
plot(rd)
}
\author{
  Matthieu Stigler <\email{Matthieu.Stigler@gmail.com}>
}

