#' Bandwidth selection for Regression Discontinuity visualisation, CTT 2015
#' 
#' Simple wrapper of the Calonico-Cattaneo-Titiunik (2015) bandwidth selection procedures 
#' for RDD visualisation \code{\link[rdrobust]{rdplot}}.
#'  
#' @param rdd_object of class rdd_data created by \code{\link{rdd_data}}
#' @param method The type of method used. See \code{\link[rdrobust]{rdplot}}. 
#' Default is \code{esmv}, the variance mimicking evenly-spaced method. 
#' @param \ldots further arguments passed to \code{\link[rdrobust]{rdplot}}. 
#' @return See documentation of \code{\link[rdrobust]{rdplot}}
#' @references Calonico, S., M. D. Cattaneo, and R. Titiunik. 2015a. Optimal Data-Driven Regression Discontinuity Plots. Journal of the American Statistical Association 110(512): 1753-1769.
#' \url{https://www.tandfonline.com/doi/abs/10.1080/01621459.2015.1017578}.
#' @seealso \code{\link{rdd_bw_ik}} Local RDD bandwidth selector using the plug-in method of Imbens and Kalyanaraman (2012)
#' @author Original code written by Calonico, Cattaneo, Farrell and Titiuni, see \code{\link[rdrobust]{rdplot}}
#' @importFrom rdrobust rdplot
#' @export
#' @examples
#' data(house)
#' rd<- rdd_data(x=house$x, y=house$y, cutpoint=0)
#' rdd_bw_cct_plot(rd)
#' 


rdd_bw_cct_plot <- function(rdd_object, method=c("esmv", "es", "espr", "esmvpr", "qs", "qspr", "qsmv", "qsmvpr"),  ...) {
  method <- match.arg(method)
  checkIsRDD(rdd_object)
  
  rdd_data <- getOriginalData(rdd_object)
  rdp <- rdrobust::rdplot(y=rdd_data$y, x=rdd_data$x, 
                           c = getCutpoint(rdd_object), hide=TRUE, 
                           ...)
  rdp
}


if(FALSE){
  # data(house)
  rd <- rdd_data(x=x, y=y, data=house, cutpoint=0)
  
  rdd_bw_cct_plot(rdd_object=rd)
  
  reg_np <- rdd_reg_np(rd)
  rdd_bw_cct_plot(reg_np)
}
