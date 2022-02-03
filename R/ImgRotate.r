library(jpeg)

#' Rotate an image 90, 180, 270, or 360 degrees counterclockwise.
#'
#' @param img, path of the input image
#' @param degree, desired degree to rotate img: 90, 180, 270, 360.
#'
#' @return rotated_img
#' @export
#'
#' @examples
#' img <- array(runif(1000),dim=c(10,10,3))
#' ImgRotate(img, 270)

ImgRotate <- function(img, degree){
    if (!is.array(img)) {
        stop("Check input the image type, it should be an array type. Try jpeg::readJPEG('image.jpeg')!")
      }
    
    if (nrow(img) <= 0 | ncol(img) <= 0) {
    stop("Invalid input image size! Double check your input is correct.")
      }
    
    if (dim(img)[3] != 3) {
    stop("Invalid image type, expecting a RGB image!")
      }
    
    if (!(degree %in% c(90, 180, 270, 360))) {
    stop("Invalid degree. Can only rotate images 90, 180, 270, or 360 degrees!")
    }

    # make copy of image
    img_rotate <- img
    
    # rotate 90 degrees counter clockwise
    if (degree == 90) {

        for (i in 1:3){
          img_rotate[,,i] <- apply(img_rotate[,,i], 1, rev)
        }
        return(img_rotate)
    }

     # rotate 180 degrees counter clockwise
    if (degree == 180) {

        for (i in 1:2) {

            for (i in 1:3){
              img_rotate[,,i] <- apply(img_rotate[,,i], 1, rev)
            }
        }
        return(img_rotate)
    }

    # rotate 270 degrees counter clockwise
    if (degree == 270) {

        for (i in 1:3){
            for (j in 1:3){
              img_rotate[,,i] <- apply(img_rotate[,,i], 1, rev)
            }
        }
        return(img_rotate)
    }
    
    # rotate 360 degrees... no rotation needed :-)
    if (degree == 360) {
    
        return(img_rotate) 
    }

}
