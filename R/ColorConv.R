library(jpeg)
#'Image Color Conversion
#'
#'This function converts an image to one of the color : gray, red, green, blue
#'
#' @param img path of the input image from package jpeg's function readJPEG().
#' @param color desired color to convert: gray, red, green, blue
#'
#' @return The converted image
#' @export
#'
#' @examples
#' img <- array(runif(1000),dim=c(10,10,3))
#' ColorConv(img, color = 'gray')
ColorConv <- function(img, color){
  TRUE
}
