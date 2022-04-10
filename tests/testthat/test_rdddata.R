# load the rddtools package
library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
rd<- rdd_data(x=house$x, y=house$y, cutpoint=0)
rd2 <- rdd_data(x=x, y=y, data=house, cutpoint=0)

# define context
context("input ambivalence")

test_that("is rd equal to rd2?", {
  expect_equal( rd, rd2)
}
)

# define context
context("rd: output format")

test_that("rd: output dimensions match", {
  expect_equal( dim(rd), c(6558, 2) )
})

test_that("rd: output values match", {
  expect_equal( rd[1   ,1],  0.1049 )
  expect_equal( rd[1   ,2],  0.581  )
  expect_equal( rd[4   ,1],  0.0868 )
  expect_equal( rd[4   ,2],  0.5846 )
  expect_equal( rd[6558,1], -0.1982 )
  expect_equal( rd[6558,2],  0.802  )
})

### use of covariates
r_cov_char <- rdd_data(y=mpg, x=wt, covar="drat", cutpoint=2, data=mtcars) 
r_cov_quote <- rdd_data(y=mpg, x=wt, covar=drat, cutpoint=2, data=mtcars) 

r_cov2_char <- rdd_data(y=mpg, x=wt, covar=c('drat', 'hp'), cutpoint=2, data=mtcars)
r_cov2_quote <- rdd_data(y=mpg, x=wt, covar=c(drat, hp), cutpoint=2, data=mtcars)
r_cov2_df <- rdd_data(y=mpg, x=wt, covar= mtcars[,c('drat', 'hp')], cutpoint=2, data=mtcars)

context("rd data: use of covariates")

test_that("rd with covars: can use char or quote", {
  expect_equal(r_cov_char, r_cov_quote)
})
test_that("rd with 2 covars: can use char or quote", {
  expect_equal(r_cov2_char, r_cov2_quote)
})
test_that("rd with 2 covars: can use char or df", {
  expect_equal(r_cov2_char, r_cov2_df)
})

