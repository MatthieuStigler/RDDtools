\name{gen_MC_IK}
\alias{gen_MC_IK}
\title{Generate Monte Carlo simulations of Imbens and Kalyanaraman}
\usage{
  gen_MC_IK(n = 200, version = 1, sd = 0.1295,
    output = c("data.frame", "RDDdata"))
}
\arguments{
  \item{n}{The size of sampel to generate}

  \item{version}{The MC version of Imbens and Kalnayaraman
  (between 1 and 4).}

  \item{sd}{The standard deviation of the error term.}

  \item{output}{Whether to return a data-frame, or already
  a RDDdata}
}
\value{
  An data frame with x and y variables.
}
\description{
  Generate the simulations reported in Imbens and
  Kalyanaraman (2012)
}
\examples{
MC1_dat <- gen_MC_IK()
MC1_rdd <- RDDdata(y=MC1_dat$y, x=MC1_dat$x, cutpoint=0)
## Use np regression:
reg_nonpara <- RDDreg_np(RDDobject=MC1_rdd)
reg_nonpara
}
\references{
  TODO
}

