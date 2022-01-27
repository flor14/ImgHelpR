library(testthat)
library(OpenImageR)
library(rprojroot)
# Set package root
root <- is_testthat
root_file <- root$make_fix_file()

image = root_file("raw_img","ubc.jpeg")
out_img <- root_file("output_img", "test_cropped_img.png")

# tests for the Crop() function

test_that("function returns the correct output shape", {
  expect_equal(dim(readImage(Crop(image, out_img, 10L, 10L)))[1:2], c(10, 10))
  expect_equal(dim(readImage(Crop(image, out_img, 15L, 15L)))[1:2], c(15, 15))
  expect_equal(dim(readImage(Crop(image, out_img, 30L, 30L)))[1:2], c(30, 30))
  
  img=Crop(image, out_img, dim(readImage(image))[1], dim(readImage(image))[2])
  expect_equal(dim(readImage(img)), dim(readImage(image)))
})


test_that("Crop function returns meaningful type error when wrong type of variables are passed in the function", {
  expect_error(Crop(10, out_img, 10, 10), "TypeError: Input path of the image should be string.")
  expect_error(Crop(image, 10, 10, 10), "TypeError: Output path of the image should be string.")
})

test_that("The input picture is converted to desired shape",{
  expect_equal(length(dim(readImage(image))), 3)
})

test_that("Crop function returns meaningful value error when wrong values are passed in the function", {
  expect_error(Crop(image, out_img, dim(readImage(image))[1]+1, 10),"ValueError: Desired height cannot exceeds original height")
  expect_error(Crop(image, out_img, 10, dim(readImage(image))[2]+1),"ValueError: Desired width cannot exceeds original width")
  
  expect_error(Crop(image, out_img, 10.5, 10),"ValueError: Height and width for the desired image must be integer")
  expect_error(Crop(image, out_img, 10, 10.5),"ValueError: Height and width for the desired image must be integer")
  
  expect_error(Crop(image, out_img, -1L, 1L), "ValueError: Height and width for the desired image must be greater than 0")
  expect_error(Crop(image, out_img, 1L, -1L), "ValueError: Height and width for the desired image must be greater than 0")
  
})

