rddtools 1.6.0
=====================
* documentation update

rddtools 1.4.0
=====================
Published on 2020-08-07

* fix CRAN error


rddtools 1.2.0
=====================
Published on 2020-07-22

* fix CRAN error

* documentation cleanup

* switch to GitHub Actions

* switch to codecov

* test using R 4.0.0


rddtools 1.0.0
=====================

* stable release

* various maintenance updates

* documentation updates 


rddtools 0.5.0
=====================

* cleanup documentation


rddtools 0.3.0
=====================

* development taken over by Bastiaan

* rename package to rddtools (from RDDtools)

* rename functions to lower case

* move package from subdir to repo root directory

* change S3class method to export for roxygen

* connect method functions with . in stead of white space

* classify default functions as RDDcoef.default etc.

* update DESCRIPTION with CRAN guidelines

* change .onLoad to .onAttach 

* remove old lyx vignette in several places

* move examples from README.Rmd to Rmd vignettes

* fix empty package dependency bug


rddtools 0.22
===========
Updated on 21/5/14

* RDDdata: change arg z to covar, add new argument z for sharp, currently unused. 

* dens_test: work now on RDDreg, return object htest

* Multiple changes in help files

* Correct import, suggests, calls to :::


rddtools 0.21
===========
Updated on 25/7/13

* Add new function RDDpred

* Add new model.matrix.RDDdata, preparing all output, now used by all RDDreg_np, RDDreg_lm, RDDgenre...

* Add method vcov.RDDreg, as.lm.RDDreg

* Add enw function vcovCluster2, complement doc, add M Arai, 

* Add data STAR_MHE

* Many small fixes


rddtools 0.2
===========
Updated on 16/7/13

* Add new option to have separate or same covariates

* Add as.nprg, to convert to a np regression from package np

* Add RDDcoef, working on multiple models (lm, np, npreg). 

* Many fixes...


rddtools 0.1
===========
Initial commit on 29/04/2013

* Initial commit, containing RDDdata, RDDreg_lm, RDDreg_np, plotSensi, plotPlacebo, etc...
