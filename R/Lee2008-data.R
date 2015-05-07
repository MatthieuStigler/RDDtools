#' @name Lee2008
#' @title Dataset used in Lee (2008)
#' @description U.S. House elections data
#' @docType data
#' @usage Lee2008
#' @description Dataset described used in Imbens and Kalyamaran (2012), and probably the same dataset used in Lee (2008),
#' @format A data frame with 6558 observations and two variables:
#' \describe{
#' \item{x}{Vote at election t-1}
#' \item{y}{Vote at election t}
#' }
#' @source Guido Imbens webpage: \url{http://scholar.harvard.edu/imbens/scholar_software/regression-discontinuity}
#' @references Imbens, Guido and Karthik Kalyanaraman. (2012) "Optimal Bandwidth Choice for the regression discontinuity estimator," 
#' Review of Economic Studies (2012) 79, 933-959
#' @references   Lee, D. (2008) Randomized experiments from non-random selection in U.S. House elections, 
#' \emph{Journal of Econometrics}, 142, 675-697
#' @examples 
#' data(Lee2008)
#' RDDlee <- rdd_data(x=x, y=y, data=Lee2008, cutpoint=0)
#' summary(RDDlee)
#' plot(RDDlee)


NULL
# Lee2008 <- read.csv("/home/mat/Dropbox/HEI/rdd/Rcode/IK bandwidth/datasets/imbens_from_MATLAB.csv", header=FALSE)
# colnames(Lee2008) <- c("x", "y")
# save(Lee2008, file="/home/mat/Dropbox/HEI/rdd/Rcode/RDDtools/data/Lee2008.rda")