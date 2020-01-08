# load the rddtools package
library(rddtools)

# load the example data set
data(house)

# create rdd_data sets
rdd_house <- rdd_data(y=house$y, x=house$x, cutpoint=0)

test_that("rd: output values match", {
  expect_equal( attributes(rdd_house)$cutpoint, 0 )
})
