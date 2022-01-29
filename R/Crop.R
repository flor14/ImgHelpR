library(jpeg)
#' Crop
#' 
#' Crop image to desired size
#'
#' @param img 
#' @param height ----Int, height of the desired image
#' @param width ----Int, width of the desired image
#'
#' @return
#' @export
#'
#' @examples
#' img <- array(runif(1000),dim=c(10,10,3))
#' Crop(img, 8,8)

Crop <- function(img, height, width){
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

  new_height = dim(img)[1] - height
  new_width = dim(img)[2] - width
  
  if(new_height %% 2 == 0){
    start_row = as.integer(new_height/2) + 1
    end_row = start_row + height - 1
  }
  else{
    start_row = as.integer((new_height -1)/2) +1
    end_row = start_row + height -1
  }
  
  if(new_width %% 2 == 0){
    start_col = as.integer(new_width/2) + 1
    end_col = start_col + width - 1
  }
  else{
    start_col = as.integer((new_width -1)/2) +1
    end_col = start_col + width -1
  }
  
  output_img <- img[start_row:end_row, start_col:end_col,]
  return(output_img)
}