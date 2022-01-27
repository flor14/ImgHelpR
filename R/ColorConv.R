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
#' 
ColorConv <- function(img, color){
  if (!is.array(img)) {
    stop("Check input image type, should be an array type. Use jpeg::readJPEG('image.jpeg').")
  }
  if (nrow(img) <= 0 | ncol(img) <= 0) {
    stop("Invalid input image size.")
  }
  if (dim(img)[3] != 3) {
    stop("Invalid image type, expecting a RGB image with 3 channel.")
  }
  if (!(tolower(color) %in% c("gray","red","green","blue"))) {
    stop("Invalid convertion color, expecting 'gray','red','green' or 'blue'.")
  }

  if (tolower(color)=="gray"){
    conv_img = mean(as.matrix(img))
    
    return(conv_img)
  
  } else if (tolower(color)=="red"){
    conv_img = img.clone()
    conv_img[,,1] = conv_img[,,1] * 0
    conv_img[,,2] = conv_img[,,2] * 0

    return(conv_img)
  
  } else if (tolower(color)=="green") {
    conv_img = img.clone()
    conv_img[,,0] = conv_img[,,0] * 0
    conv_img[,,2] = conv_img[,,2] * 0
    
    return(conv_img)
  
  } else if (tolower(color)=="blue") {
    conv_img = img.clone()
    conv_img[,,0] = conv_img[,,0] * 0
    conv_img[,,1] = conv_img[,,1] * 0
    
    return(conv_img)
  }
}
