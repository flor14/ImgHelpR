library(testthat)
# tests for the Crop() function
img <- array(runif(1000),dim=c(50,50,3))
img2 <- Crop(img,8,8)

test_that("function returns the correct output shape", {
  expect_equal(dim(Crop(img, 10, 10))[1:2], c(10, 10))
  expect_equal(dim(Crop(img, 15, 15))[1:2], c(15, 15))
  expect_equal(dim(Crop(img, 30, 30))[1:2], c(30, 30))
})

test_that("The input picture is converted to desired shape",{
  expect_equal(length(dim(img)), 3)
})

test_that("Crop function returns meaningful value error when wrong values are passed in the function", {
  expect_error(Crop(img, dim(img)[1]+1, 10),"ValueError: Desired height cannot exceeds original height")
  expect_error(Crop(img, 10, dim(img)[2]+1),"ValueError: Desired width cannot exceeds original width")
  
  expect_error(Crop(img, 10.5, 10),"ValueError: Height and width for the desired image must be integer")
  expect_error(Crop(img, 10, 10.5),"ValueError: Height and width for the desired image must be integer")
  
  expect_error(Crop(img, -1L, 1L), "ValueError: Height and width for the desired image must be greater than 0")
  expect_error(Crop(img, 1L, -1L), "ValueError: Height and width for the desired image must be greater than 0")
})