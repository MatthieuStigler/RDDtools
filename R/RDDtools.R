#' Regression Discontinuity Design 
#' 
#' Provides function to do a comprehensive regression discontinuity analysis. 
#' 
#' @name RDDtools-package
#' @aliases RDDtools
#' @docType package
#' @import KernSmooth
#' @import np
#' @import ggplot2
#' @title Regression Discontinuity Design 
#' @author Matthieu Stigler <\email{Matthieu.Stigler@@gmail.com}>

if (getRversion() >= "3.1.0") {
  utils::globalVariables(c("x", "y", "position", "cutpoint", "LATE", "CI_low", "CI_high"))
  utils::suppressForeignCheck(c("x", "y", "position", "cutpoint", "LATE", "CI_low", "CI_high"))
}