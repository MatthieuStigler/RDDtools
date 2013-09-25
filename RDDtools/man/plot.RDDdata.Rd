\name{plot.RDDdata}
\alias{plot.RDDdata}
\title{Plot RDDdata}
\usage{
  \method{plot}{RDDdata} (x, h, nbins = NULL,
    xlim = range(object$x, na.rm = TRUE), cex = 0.7,
    nplot = 1, device = c("base", "ggplot"), ...)
}
\arguments{
  \item{x}{Object of class RDDdata}

  \item{h}{The binwidth parameter (note this differs from
  the bandwidth parameter!)}

  \item{nbins}{Alternative to h, the total number of bins
  in the plot.}

  \item{xlim}{The range of the x data}

  \item{cex}{Size of the points, see \code{\link{par}}}

  \item{nplot}{Number of plot to draw}

  \item{device}{Type of device used. Currently not used.}

  \item{\ldots}{Further arguments passed to the
  \code{\link{plot}} function.}
}
\value{
  A plot
}
\description{
  Binned plot of the forcing and outcome variable
}
\examples{
data(Lee2008)
rd<- RDDdata(x=Lee2008$x, y=Lee2008$y, cutpoint=0)

plot(rd)
}
\author{
  Matthieu Stigler <\email{Matthieu.Stigler@gmail.com}>
}

