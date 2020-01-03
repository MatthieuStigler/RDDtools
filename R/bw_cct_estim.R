#' Bandwidth selection for Regression Discontinuity estimators, CTT 2014
#' 
#' Simple wrapper of the Calonico-Cattaneo-Titiunik (2014) bandwidth selection procedures 
#' for RDD estimators \code{\link[rdrobust]{rdbwselect}}.
#' 
#' @param rdd_object of class rdd_data created by \code{\link{rdd_data}}
#' @param kernel The type of kernel used: either \code{Triangular}, \code{Uniform} or \code{Epanechnikov}. 
#' @param method The type of method used. See 
#' @param \ldots further arguments passed to \code{\link[rdrobust]{rdbwselect}}. 
#' @return See documentation of \code{\link[rdrobust]{rdbwselect}}
#' @references Calonico, S., M. D. Cattaneo, and R. Titiunik. 2014a. Robust Nonparametric Confidence Intervals for Regression-Discontinuity Designs. Econometrica 82(6): 2295-2326.
#' \url{https://www.tandfonline.com/doi/abs/10.1080/01621459.2015.1017578}.
#' @seealso \code{\link{rdd_bw_ik}} Local RDD bandwidth selector using the plug-in method of Imbens and Kalyanaraman (2012)
#' @author Original code written by Calonico, Cattaneo, Farrell and Titiuni, see \code{\link[rdrobust]{rdbwselect}}
#' @importFrom rdrobust rdbwselect
#' @export
#' @examples
#' data(house)
#' rd<- rdd_data(x=house$x, y=house$y, cutpoint=0)
#' rdd_bw_cct_estim(rd)
#' 



rdd_bw_cct_estim <- function(rdd_object,
                       method=c("mserd", "msetwo", "msesum", "msecomb1", "msecomb2", "cerrd", "certwo", "cersum", "cercomb1"),
                       kernel = c("Triangular", "Uniform", "Epanechnikov"), ...) {

  kernel <- tolower(match.arg(kernel))
  method <- match.arg(method)

  checkIsRDD(rdd_object)

  rdd_data <- getOriginalData(rdd_object)
  
  res <- rdrobust::rdbwselect(y=rdd_data$y, x=rdd_data$x, 
             c = getCutpoint(rdd_object), 
             kernel = "tri", 
             bwselect = method,
             ...)
  return(res)
}
