test_that("Error should be thrown when input is not array type", {
  expect_error(ImgCompress(c(1,2,3),method="svd",level=1))
})

test_that("Error should be thrown when input image size is not valid", {
  expect_error(ImgCompress(array(runif(1000),dim=c(0,0,3)),method="svd",level=1))
})

test_that("Error should be thrown when input method is not expected", {
  expect_error(ImgCompress(array(runif(1000),dim=c(10,10,3)),method="apple",level=1))
})

test_that("Error should be thrown when input level is not expected", {
  expect_error(ImgCompress(array(runif(1000),dim=c(10,10,3)),method="svd",level=5))
})

test_that("Validate function svd method output is correct", {
  expect_equal(ImgCompress(array(1000,dim=c(10,10,3)),method="svd",level=1), array(rep(1,1000),dim=c(10,10,3)))
})

test_that("Validate function resize output is correct", {
  expect_equal(dim(ImgCompress(array(1000,dim=c(10,10,3)),method="resize",level=2)), c(5, 5, 3))
})
