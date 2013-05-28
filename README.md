RDDtools: an R package for Regression Discontinuity Design
========================================================

**RDDtools** is a new R package under development, designed to offer a set of tools to run all the steps required for a Regression Discontinuity Design (RDD) Analysis, from primary data visualisation to discontinuity estimation, sensitivity and placebo testing. 


Installing **RDDtools**
-----------------------

This github website hosts the source code. One of the easiest ways to install the package from github is by using the R package **devtools**:


```r
library(devtools)
install_github(repo = "RDDtools", username = "MatthieuStigler", subdir = "RDDtools")
```


Note however the latest version of RDDtools only works with R 3.0, and that you might need to install  [Rtools](http://stat.ethz.ch/CRAN/bin/windows/Rtools/) if on Windows. 


Using RDDtools: a quick example
-----------------------

**RDDtools** works in an object-oriented way: the user has to define once the characteristic of the data, creating a *RDDdata* object, on which different anaylsis tools can be applied, such as:
+  Simple visualisation of the data using binned-plot: **plot()**
+  RDD parametric estimation: **RDDreg_lm()**
+  RDD local non-parametric estimation: **RDDreg_np()**
+  Plot of sensitivity to bandwidth: **plotSensi()**
+  Placebo plot using different cutpoints: **plotSensi()**


Load the package, and load the built-in dataset from Lee (2008):






```r
library(RDDtools)
data(Lee2008)
```


Declare the data as a RDDdata object:


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

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



As well as run a simple local regression, using the Imbens and Kalyanaraman bandwidth:

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
##   Estimate Std. Error t value Pr(>|t|)    
## D  0.07992    0.00682    11.7   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
plot(x = reg_nonpara)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 



One can easily check the sensitivity of the estimate to different bandwidths:

```r
plotSensi(reg_nonpara, from = 0.05)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



