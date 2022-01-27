test_that("Error should be thrown when input is not array type", {
  expect_error(ColorConv(c(1,2,3),color="red"))
})

test_that("Error should be thrown when input image size is not valid", {
  expect_error(ColorConv(array(runif(1000),dim=c(0,0,3)),method="red"))
})

test_that("Error should be thrown when input image do not have 3 channel", {
  expect_error(ColorConv(array(runif(1000),dim=c(10,10,1)),method="red"))
})

test_that("Error should be thrown when input color is not expected", {
  expect_error(ColorConv(array(runif(1000),dim=c(10,10,3)),color ="yellow"))
})

test_that("Validate function red color output is correct", {
  expect_equal(ColorConv(array(1000,dim=c(10,10,3)),color ="red")[,,1], array(0,dim=c(10,10,1)))
  expect_equal(ColorConv(array(1000,dim=c(10,10,3)),color ="red")[,,2], array(0,dim=c(10,10,1)))
})

