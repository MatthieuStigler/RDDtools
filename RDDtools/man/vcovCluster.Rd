\name{vcovCluster}
\alias{vcovCluster}
\title{Cluster Heteroskedasticity-consistent estimation of the covariance matrix.}
\usage{
  vcovCluster(object, clusterVar)
}
\arguments{
  \item{object}{Object of class lm, from which RDDreg also
  inherits.}

  \item{clusterVar}{The variable containing the cluster
  attributions.}
}
\value{
  A matrix containing the covariance matrix estimate.
}
\description{
  Offer a cluster variant of the usual
  Heteroskedasticity-consistent
}
\examples{
data(STAR_MHE)
if(all(c(require(sandwich), require(lmtest)))){

# Run simple regression:
reg_krug <- lm(pscore~cs, data=STAR_MHE)

# Row 1 of Table 8.2.1, inference with standard vcovHC:
coeftest(reg_krug,vcov.=vcovHC(reg_krug, "HC1"))[2,2]

# Row 4 of Table 8.2.1, inference with cluster vcovHC:
coeftest(reg_krug,vcov.=vcovCluster(reg_krug, clusterVar=STAR_MHE$classid))[2,2]
}
}
\author{
  Mahmood Arai, see \url{http://people.su.se/~ma/clmclx.R}
}
\seealso{
  \code{\link{clusterInf}} for a direct function, allowing
  also alternative cluster inference methods.
}

