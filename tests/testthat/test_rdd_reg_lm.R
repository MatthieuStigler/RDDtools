# load the rddtools package
library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
r_cov_char <- rdd_data(y=mpg, x=wt, covar="drat", cutpoint=3, data=mtcars) 

context("Arguments of rdd_reg_lm")

test_that("rdd_reg_lm args: covar.opt should be a list", {
  expect_error(rdd_reg_lm(r_cov_char, order=1, covar.opt = 'include'),
               "Argument 'covar.opt' should be a list")
})
