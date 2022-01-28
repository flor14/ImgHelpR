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
