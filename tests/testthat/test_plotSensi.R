# load the rddtools package
library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
reg_nonpara <- rdd_reg_np(rdd_object=rdd_data(y=house$y, x=house$x, cutpoint=0) )

# store plot in object
sensiplot <- plotSensi(reg_nonpara)

test_that("rd: output values match", {
  expect_equal( length(sensiplot),  6 )
})
