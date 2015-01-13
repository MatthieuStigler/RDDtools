#' @name STAR_MHE
#' @title Transformation of the STAR dataset as used in Angrist and Pischke (2008)
#' @description Transformation of the STAR dataset as used in Table 8.2.1 of Angrist and Pischke (2008) 
#' @docType data
#' @usage STAR_MHE
#' @seealso \code{\link[AER]{STAR}} for the original dataset.
#' @format A data frame containing 5743 observations and 6 variables. The first variable is from the original dataset, 
#' all other are created by Angrist and Pischke STAT code.
#' \describe{
#' \item{schidkn}{School ID in kindergarden (original variable, schoolidk in \code{\link[AER]{STAR}})}
#' \item{pscore}{The propensity score  (computed by A & P)}
#' \item{classid}{The id of the class (computed by A & P)}
#' \item{cs}{Class size (computed by A & P)}
#' \item{female, nwhite}{Various covariates (computed by A & P)}
#' }
#' @details ). This is a transformation of the dataset from the project STAR (Student/Teacher Achievement Ratio. 
#' The full dataset is described and available in package AER, \code{\link[AER]{STAR}}. 
#' The transformed data was obtained using the STATA script krueger.do, obtained from Joshua Angrist website 
#' (\url{http://economics.mit.edu/faculty/angrist/data1/mhe/krueger}), on the webstar.dta.
#' @references Krueger, A. (1999) "Experimental Estimates Of Education Production Functions," 
#' \emph{The Quarterly Journal of Economics}, Vol. 114(2), pages 497-532, May.
#' @references Angrist, A. ad  Pischke J-S (2008) \emph{Mostly Harmless Econometrics: An Empiricist's Companion}, 
#' Princeton University press
#' @source Data obtained using the script krueger.do on data webstar.rda, found on J. Angrist website 
#' \url{http://economics.mit.edu/faculty/angrist/data1/mhe/krueger}, retrieved on 26 November 2012.
#' @examples 
#' data(STAR_MHE)
#' 
#' # Compute the group means:
#' STAR_MHE_means <- aggregate(STAR_MHE[, c("classid", "pscore", "cs")], by=list(STAR_MHE$classid), mean)
#' 
#' # Regression of means, with weighted average:
#' reg_krug_gls <- lm(pscore~cs, data=STAR_MHE_means, weights=cs)
#' coef(summary(reg_krug_gls))[2,2]

NULL


##### Quick R code used on the output data:
# STAR_MHE <- read.csv(".../abuelita.csv")
# STAR_MHE$female <- as.factor(STAR_MHE$female)
# STAR_MHE$nwhite <- as.factor(STAR_MHE$nwhite)
# STAR_MHE$n <- NULL
# 
# save(STAR_MHE, file="STAR_MHE.rda")


##### STATA code krueger.do (retrieved 26 November 2012 on http://economics.mit.edu/faculty/angrist/data1/mhe/krueger)
# version 9
# set more 1
# capture log close
# log using krueger, text replace
# 
# /* create Krueger scaled scores */
#   
#   /* reading score */
#   
#   clear
# use webstar
# 
# keep if cltypek > 1  		/* regular classes */
#   keep if treadssk ~= .
# 
# sort treadssk
# gen pread0 = 100*_n/_N
# 
# egen pread = mean(pread0), by(treadssk)	/* percentile score in reg. classes */
#   
#   keep treadssk pread
# sort tread
# keep if tread ~= tread[_n-1]
# save tempr, replace
# 
# /* math score */
#   
#   use webstar
# 
# keep if cltypek > 1			/* regular classes */
#   keep if tmathssk ~= .
# 
# sort tmathssk
# gen pmath0 = 100*_n/_N
# egen pmath = mean(pmath0), by(tmathssk)
# 
# keep tmathssk pmath
# sort tmath
# keep if tmath ~= tmath[_n-1]
# save tempm, replace
# 
# /* merge percentile scores back on */
#   
#   use webstar
# 
# keep if stark == 1
# 
# sort treadssk
# merge treadssk using tempr
# ipolate pread treadssk, gen(pr) epolate
# drop _merge
# 
# sort tmathssk
# merge tmathssk using tempm
# ipolate pmath tmathssk, gen(pm) epolate
# replace pm = 0 if pm < 0
# drop _merge
# 
# egen pscore = rowmean(pr pm)
# 
# /* make class ids */
#   
#   egen classid1 = group(schidkn cltypek)
# egen cs1 = count(classid1), by(classid1)
# 
# egen classid2 = group(classid1 totexpk hdegk cladk) if cltypek==1 & cs >= 20
# egen classid3 = group(classid1 totexpk hdegk cladk) if cltypek>1 & cs >= 30
# 
# gen temp = classid1*100
# egen classid = rowtotal(temp classid2 classid3)
# egen cs = count(classid), by(classid)
# 
# gen female = ssex == 2
# gen nwhite = srace >= 2 & srace <= 6 if srace ~= .
# 
# keep if cs <= 27 & pscore ~= .
# keep pscore cs schidkn classid female nwhite
# gen n = 1
# 
# save temp, replace
# 
# reg pscore cs, robust
# local se = _se[cs]
# local t = _b[cs]/`se'
# predict r, res
# loneway r classid
# local rho = r(rho)
# 
# collapse cs, by(classid)
# sum cs
# 
# dis r(Var)
# local m = 1 + (r(Var)/r(mean) + r(mean) - 1)*`rho'
# dis `m'
# dis sqrt(`m')
# dis `se'
# dis sqrt(`m')*`se'
# dis `t'/sqrt(`m')
# 
# 
# use temp, clear
# 
# reg pscore cs, robust
# moulton pscore cs, cluster(classid) moulton
# moulton pscore cs, cluster(classid)
# reg pscore cs, cluster(classid)
# brl pscore cs, cluster(classid)
# 
# 
# 
# set seed 123456789
# bootstrap "reg pscore cs" _b, reps(1000) cluster(classid)
# 
# areg pscore, absorb(classid)
# predict hat
# gen ry = pscore - hat + _b[_cons]
# collapse (mean) ry cs (sum) n, by(classid)
# 
# reg ry cs [aw=n]
# 
# 
# log close
# set more 0
