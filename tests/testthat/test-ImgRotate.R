library(testthat)
# should be array
test_that("Error should be thrown when input is not array type", {
  expect_error(ImgRotate(c(1,2,3), 180))
})

# test var for wrong image size
wrong_dim <- array(c(1:15),dim = c(3,3,0))

test_that("Error should be thrown when array is not 3D/RGB image!",{
    expect_error(ImgRotate(wrong_dim,360))
})

# test var for wrong dimensions
wrong_size <- array(c(1:15),dim = c(0,0,0))

test_that("Error should be thrown when array is not 3D/RGB image!",{
    expect_error(ImgRotate(wrong_dim ,360))
})

# test for incorrect degree input
test_that("Error should be thrown when degree is not in [90, 180, 270, 360]", {
  expect_error(ImgRotate(img,299.4))
})


test_that("Validate function resize output is correct", {
  expect_equal(ImgRotate(array(c(1,2,3,4,5,6,7,8,9),dim=c(3,3,3)),90),array(c(7,4,1,8,5,2,9,6,3),dim=c(3,3,3)))
  expect_equal(ImgRotate(array(c(1,2,3,4,5,6,7,8,9),dim=c(3,3,3)),180),array(c(9,8,7,6,5,4,3,2,1),dim=c(3,3,3)))
  expect_equal(ImgRotate(array(c(1,2,3,4,5,6,7,8,9),dim=c(3,3,3)),270),array(c(3,6,9,2,5,8,1,4,7),dim=c(3,3,3)))
  expect_equal(ImgRotate(array(c(1,2,3,4,5,6,7,8,9),dim=c(3,3,3)),360),array(c(1,2,3,4,5,6,7,8,9),dim=c(3,3,3)))
})
