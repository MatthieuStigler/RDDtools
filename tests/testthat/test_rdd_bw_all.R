# load the  package
library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
rd <- rdd_data(x=x, y=y, data=house, cutpoint=0)
reg_np <- rdd_reg_np(rd)

# define context
context("rd: output format")

test_that("bw work on direct data and reg output", {
  expect_equal(rdd_bw_cct_plot(rdd_object=rd),
                rdd_bw_cct_plot(reg_np) )
  expect_equal(rdd_bw_cct_estim(rdd_object=rd),
               rdd_bw_cct_estim(reg_np) )
  expect_equal(rdd_bw_ik(rdd_object=rd),
               rdd_bw_ik(reg_np) )
})

