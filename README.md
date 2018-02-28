rddtools
========================================================
[![License](http://img.shields.io/badge/license-GPLv3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![CRAN Version](http://www.r-pkg.org/badges/version/rddtools)](https://cran.r-project.org/package=rddtools)
[![Total RStudio Cloud Downloads](http://cranlogs.r-pkg.org/badges/grand-total/rddtools?color=brightgreen)](https://cran.rstudio.com/web/packages/rddtools/index.html)
[![RStudio Cloud Downloads](http://cranlogs.r-pkg.org/badges/rddtools?color=brightgreen)](http://cran.r-project.org/package=rddtools)
[![Travis-CI Build Status](https://travis-ci.org/bquast/rddtools.png?branch=master)](https://travis-ci.org/bquast/rddtools)
[![Coverage Status](https://coveralls.io/repos/bquast/rddtools/badge.svg)](https://coveralls.io/r/bquast/rddtools)

**rddtools** is a new R package under development, designed to offer a set of tools to run all the steps required for a Regression Discontinuity Design (RDD) Analysis, from primary data visualisation to discontinuity estimation, sensitivity and placebo testing. 


Installing **rddtools**
-----------------------

This github website hosts the source code. One of the easiest ways to install the package from github is by using the R package **devtools**:

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github( "bquast/rddtools" )
```

Note however the latest version of rddtools only works with R 3.0, and that you might need to install  [Rtools](http://stat.ethz.ch/CRAN/bin/windows/Rtools/) if on Windows. 


Documentation
-----------------------
The (preliminary) documentation is available in the help files directly, as well as in the *vignettes*. The vignettes can be accessed from R.

```r
vignette("rddtools")
```

rddtools: main features
-----------------------

+  Simple visualisation of the data using binned-plot: `plot()`

+ Bandwidth selection:
  + MSE-RDD bandwidth procedure of [Imbens and Kalyanaraman 2012]: `rdd_bw_ik()`
  + MSE global bandwidth procedure of [Ruppert et al 1995]: `rdd_bw_rsw()`
+ Estimation:
  +  RDD parametric estimation: `rdd_reg_lm()` This includes specifying the polynomial order, including covariates with various specifications as advocated in [Imbens and Lemieux 2008].
  +  RDD local non-parametric estimation: `rdd_reg_np()`. Can also include covariates, and allows different types of inference (fully non-parametric, or parametric approximation). 
  +  RDD generalised estimation: allows to use custom estimating functions to get the RDD coefficient. Could allow for example a probit RDD, or quantile regression.
+ Post-Estimation tools:
  + Various tools, to obtain predictions at given covariate values ( `rdd_pred()` ), or to convert to other classes, to lm ( **as.lm()** ), or to the package `np` ( `as.npreg()` ). 
  + Function to do inference with clustered data: `clusterInf()` either using a cluster covariance matrix ( **vcovCluster()** ) or by a degrees of freedom correction (as in [Cameron et al. 2008]).
+ Regression sensitivity analysis:
  + Plot the sensitivity of the coefficient with respect to the bandwith: `plotSensi()`
  + *Placebo plot* using different cutpoints: `plotPlacebo()`
+ Design sensitivity analysis:
  + McCrary test of manipulation of the forcing variable: wrapper `dens_test()` to the function `DCdensity()` from package `rdd`. 
  + Test of equal means of covariates: `covarTest_mean()`
  + Test of equal density of covariates: `covarTest_dens()`
+ Datasets
  + Contains the seminal dataset of [Lee 2008]: `house`
  + Contains functions to replicate the Monte-Carlo simulations of [Imbens and Kalyanaraman 2012]: `gen_mc_ik()`

References
-----------------------
  [Imbens and Kalyanaraman 2012]: http://ideas.repec.org/a/oup/restud/v79y2012i3p933-959.html "Imbens, G. & Kalyanaraman, K. (2012) Optimal Bandwidth Choice for the Regression Discontinuity Estimator, Review of Economic Studies, 79, 933-959"
  
  [Lee 2008]: http://ideas.repec.org/a/eee/econom/v142y2008i2p675-697.html "Lee, D. S. (2008) Randomized experiments from non-random selection in U.S. House elections, Journal of Econometrics, 142, 675-697"
  
  [Imbens and Lemieux 2008]: http://ideas.repec.org/a/eee/econom/v142y2008i2p615-635.html "Imbens, G. & Lemieux, T. (2008) Regression discontinuity designs: A guide to practice, Journal of Econometrics, Vol. 142(2), pages 615-635"
  
  [Cameron et al. 2008]: http://ideas.repec.org/a/tpr/restat/v90y2008i3p414-427.html "Cameron, Gelbach and Miller (2008) Bootstrap-Based Improvements for Inference with Clustered Errors, The Review of Economics and Statistics, Vol. 90(3), pages 414-427"
  
  [Ruppert et al 1995]: http://www.jstor.org/stable/2291516 "Ruppert, D., Sheather, S. J. and Wand, M. P. (1995). An effective bandwidth selector for local least squares regression. Journal of the American Statistical Association, 90, 1257–1270."
