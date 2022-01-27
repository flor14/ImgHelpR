library(OpenImageR)
library(numbers)
library(png)
library(testthat)
library(assertthat)
#' Title Crop
#'
#' @param input_path ----String, path of the input image
#' @param output_path  ----String, path of the output image
#' @param height ----Int, height of the desired image
#' @param width ----Int, width of the desired image
#'
#' @import numbers OpenImageR testthat assertthat png rprojroot

Crop <- function(input_path, output_path, height, width){
  if (assertthat::is.string(input_path) == FALSE){
    stop("TypeError: Input path of the image should be string.")
  }
  if (assertthat::is.string(output_path) == FALSE){
    stop("TypeError: Output path of the image should be string.")
  }
  img <- readImage(input_path)
  if (height > dim(img)[1]){
    stop("ValueError: Desired height cannot exceeds original height")
  }
  if (width > dim(img)[2]){
    stop("ValueError: Desired width cannot exceeds original width")
  }
  if (height%%1 != 0 || width%%1 != 0){
    stop("ValueError: Height and width for the desired image must be integer")
  }
  if(height<=0 || width <=0){
    stop("ValueError: Height and width for the desired image must be greater than 0")
  }
  print("Cropping the image...")
  
  new_height = dim(img)[1] - height
  new_width = dim(img)[2] - width
  
  if(rem(new_height, 2) == 0){
    start_row = as.integer(new_height/2) + 1
    end_row = start_row + height - 1
  }
  else{
    start_row = as.integer((new_height -1)/2) +1
    end_row = start_row + height -1
  }
  
  if(rem(new_width, 2) == 0){
    start_col = as.integer(new_width/2) + 1
    end_col = start_col + width - 1
  }
  else{
    start_col = as.integer((new_width -1)/2) +1
    end_col = start_col + width -1
  }
  
  output_img <- img[start_row:end_row, start_col:end_col,]
  save <-writePNG(output_img, target = output_path)
  print(paste0("Saving cropped image to ", output_path))
  return(output_path)
  
}