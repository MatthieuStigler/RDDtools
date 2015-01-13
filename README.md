RDDtools: an R package for Regression Discontinuity Design
========================================================

**RDDtools** is a new R package under development, designed to offer a set of tools to run all the steps required for a Regression Discontinuity Design (RDD) Analysis, from primary data visualisation to discontinuity estimation, sensitivity and placebo testing. 


Installing **RDDtools**
-----------------------

This github website hosts the source code. One of the easiest ways to install the package from github is by using the R package **devtools**:


```r
library(devtools)
install_github(repo = "MatthieuStigler/RDDtools")
```


Note however the latest version of RDDtools only works with R 3.0, and that you might need to install  [Rtools](http://stat.ethz.ch/CRAN/bin/windows/Rtools/) if on Windows. 


Documentation
-----------------------
The (preliminary) documentation is available in the help files directly, as well as in the *vignette*. The vignette can be accessed from R with vignette("RDDtools"), or by accessing the [pdf](https://github.com/MatthieuStigler/RDDtools/raw/master/RDDtools/inst/doc/RDDtools.pdf) stored on this github. 

RDDtools: main features
-----------------------


+  Simple visualisation of the data using binned-plot: **plot()**

+ Bandwidth selection:
  + MSE-RDD bandwidth procedure of [Imbens and Kalyanaraman 2012]: **RDDbw_IK()**
  + MSE global bandwidth procedure of [Ruppert et al 1995]: **RDDbw_RSW()**
+ Estimation:
  +  RDD parametric estimation: **RDDreg_lm()** This includes specifying the polynomial order, including covariates with various specifications as advocated in [Imbens and Lemieux 2008].
  +  RDD local non-parametric estimation: **RDDreg_np()**. Can also include covariates, and allows different types of inference (fully non-parametric, or parametric approximation). 
  +  RDD generalised estimation: allows to use custom estimating functions to get the RDD coefficient. Could allow for example a probit RDD, or quantile regression.
+ Post-Estimation tools:
  + Various tools, to obtain predictions at given covariate values ( **RDDpred()** ), or to convert to other classes, to lm ( **as.lm()** ), or to the package *np* ( **as.npreg()** ). 
  + Function to do inference with clustered data: **clusterInf()** either using a cluster covariance matrix ( **vcovCluster()** ) or by a degrees of freedom correction (as in [Cameron et al. 2008]).
+ Regression sensitivity analysis:
  + Plot the sensitivity of the coefficient with respect to the bandwith: **plotSensi()**
  + *Placebo plot* using different cutpoints: **plotPlacebo()**
+ Design sensitivity analysis:
  + McCrary test of manipulation of the forcing variable: wrapper **dens_test()** to the function **DCdensity()** from package **rdd**. 
  + Test of equal means of covariates: **covarTest_mean()**
  + Test of equal density of covariates: **covarTest_dens()**
+ Datasets
  + Contains the seminal dataset of [Lee 2008]: **Lee2008**
  + Contains functions to replicate the Monte-Carlo simulations of [Imbens and Kalyanaraman 2012]: **gen_MC_IK()**

Using RDDtools: a quick example
-----------------------
**RDDtools** works in an object-oriented way: the user has to define once the characteristic of the data, creating a *RDDdata* object, on which different anaylsis tools can be applied. 

### Data preparation and visualisation
Load the package, and load the built-in dataset from [Lee 2008]:






```r
library(RDDtools)
data(Lee2008)
```


Declare the data to be a *RDDdata* object:


```r
Lee2008_rdd <- RDDdata(y = Lee2008$y, x = Lee2008$x, cutpoint = 0)
```



You can now directly summarise and visualise this data:


```r
summary(Lee2008_rdd)
```

```
## ### RDDdata object ###
## 
## Cutpoint: 0 
## Sample size: 
## 	-Full : 6558 
## 	-Left : 2740 
## 	-Right: 3818
## Covariates: no
```

```r
plot(Lee2008_rdd)
```

<img src="figuresREADME/dataPlot.png" title="plot of chunk dataPlot" alt="plot of chunk dataPlot" style="display: block; margin: auto;" />


### Estimation

#### Parametric

Estimate parametrically, by fitting a 4th order polynomial:

```r
reg_para <- RDDreg_lm(RDDobject = Lee2008_rdd, order = 4)
reg_para
```

```
## ### RDD regression: parametric ###
## 	Polynomial order:  4 
## 	Slopes:  separate 
## 	Number of obs: 6558 (left: 2740, right: 3818)
## 
## 	Coefficient:
##   Estimate Std. Error t value Pr(>|t|)    
## D   0.0766     0.0132    5.79  7.6e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r

plot(reg_para)
```

<img src="figuresREADME/reg_para.png" title="plot of chunk reg_para" alt="plot of chunk reg_para" style="display: block; margin: auto;" />



#### Non-parametric
As well as run a simple local regression, using the [Imbens and Kalyanaraman 2012] bandwidth:

```r
bw_ik <- RDDbw_IK(Lee2008_rdd)
reg_nonpara <- RDDreg_np(RDDobject = Lee2008_rdd, bw = bw_ik)
print(reg_nonpara)
```

```
## ### RDD regression: nonparametric local linear###
## 	Bandwidth:  0.2939 
## 	Number of obs: 3200 (left: 1594, right: 1606)
## 
## 	Coefficient:
##   Estimate Std. Error z value Pr(>|z|)    
## D  0.07992    0.00946    8.44   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
plot(x = reg_nonpara)
```

<img src="figuresREADME/RegPlot.png" title="plot of chunk RegPlot" alt="plot of chunk RegPlot" style="display: block; margin: auto;" />


### Regression Sensitivity tests:

One can easily check the sensitivity of the estimate to different bandwidths:

```r
plotSensi(reg_nonpara, from = 0.05, to = 1, by = 0.1)
```

<img src="figuresREADME/SensiPlot.png" title="plot of chunk SensiPlot" alt="plot of chunk SensiPlot" style="display: block; margin: auto;" />


Or run the Placebo test, estimating the RDD effect based on fake cutpoints:

```r
plotPlacebo(reg_nonpara)
```

<img src="figuresREADME/placeboPlot.png" title="plot of chunk placeboPlot" alt="plot of chunk placeboPlot" style="display: block; margin: auto;" />


### Design Sensitivity tests:

Design sensitivity tests check whether the discontinuity found can actually be attributed ot other causes. Two types of tests are available:

+ Discontinuity comes from manipulation: test whether there is possible manipulation around the cutoff, McCrary 2008 test: **dens_test()**
+ Discontinuity comes from other variables: should test whether discontinuity arises with covariates. Currently, only simple tests of equality of covariates around the threshold are available: 

#### Discontinuity comes from manipulation: McCrary test

use simply the function **dens_test()**, on either the raw data, or the regression output:

```r
dens_test(reg_nonpara)
```

<img src="figuresREADME/DensPlot.png" title="plot of chunk DensPlot" alt="plot of chunk DensPlot" style="display: block; margin: auto;" />

```
## 
## 	McCrary Test for no discontinuity of density around cutpoint
## 
## data:  reg_nonpara
## z-val = 1.295, p-value = 0.1952
## alternative hypothesis: Density is discontinuous around cutpoint
## sample estimates:
## Discontinuity 
##        0.1035
```


#### Discontinuity comes from covariates: covariates balance tests

Two tests available:
+ equal means of covariates: **covarTest_mean()**
+ equal density of covariates: **covarTest_dens()**


We need here to simulate some data, given that the Lee (2008) dataset contains no covariates.
We here simulate three variables, with the second having a different mean on the left and the right. 


```r
set.seed(123)
n_Lee <- nrow(Lee2008)
Z <- data.frame(z1 = rnorm(n_Lee, sd = 2), z2 = rnorm(n_Lee, mean = ifelse(Lee2008 < 
    0, 5, 8)), z3 = sample(letters, size = n_Lee, replace = TRUE))
Lee2008_rdd_Z <- RDDdata(y = Lee2008$y, x = Lee2008$x, covar = Z, cutpoint = 0)
```



Run the tests:

```r
## test for equality of means around cutoff:
covarTest_mean(Lee2008_rdd_Z, bw = 0.3)
```

```
##    mean of x mean of y Difference statistic p.value
## z1 0.004268  0.02186   0.01759    -0.2539   0.7996 
## z2 5.006     7.985     2.979      -84.85    0      
## z3 13.19     13.44     0.2465     -0.941    0.3468
```

```r

## Can also use function covarTest_dis() for Kolmogorov-Smirnov test:
covarTest_dis(Lee2008_rdd_Z, bw = 0.3)
```

```
##    statistic p.value
## z1 0.03482   0.2727 
## z2 0.8648    0      
## z3 0.03009   0.4474
```


Tests correctly reject equality of the second, and correctly do not reject equality for the first and third. 

  [Imbens and Kalyanaraman 2012]: http://ideas.repec.org/a/oup/restud/v79y2012i3p933-959.html "Imbens, G. & Kalyanaraman, K. (2012) Optimal Bandwidth Choice for the Regression Discontinuity Estimator, Review of Economic Studies, 79, 933-959"
  
  [Lee 2008]: http://ideas.repec.org/a/eee/econom/v142y2008i2p675-697.html "Lee, D. S. (2008) Randomized experiments from non-random selection in U.S. House elections, Journal of Econometrics, 142, 675-697"
  
  [Imbens and Lemieux 2008]: http://ideas.repec.org/a/eee/econom/v142y2008i2p615-635.html "Imbens, G. & Lemieux, T. (2008) Regression discontinuity designs: A guide to practice, Journal of Econometrics, Vol. 142(2), pages 615-635"
  
  [Cameron et al. 2008]: http://ideas.repec.org/a/tpr/restat/v90y2008i3p414-427.html "Cameron, Gelbach and Miller (2008) Bootstrap-Based Improvements for Inference with Clustered Errors, The Review of Economics and Statistics, Vol. 90(3), pages 414-427"
  
  [Ruppert et al 1995]: http://www.jstor.org/stable/2291516 "Ruppert, D., Sheather, S. J. and Wand, M. P. (1995). An effective bandwidth selector for local least squares regression. Journal of the American Statistical Association, 90, 1257–1270."


  
