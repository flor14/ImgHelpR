#'Image Compression
#'
#'Image compression using different compression method.
#'This function compress image to a user defined compression level.
#'
#'SVD Method:
#'  SVD (Singular Value Decomposition) on RGB image. 
#'Note: Actual image memory size is not changed from this function.
#'It does not handle the Jpeg compression algorithm
#'
#'Resize:
#'  Resize image to different size using interval pixel selection method.
#'
#' @param img m x n Image Matrix converted from package jpeg's function readJPEG().
#' @param method Compression methods: Resize, SVD
#' @param level Level of compression: 1-High, 2-Med, 3-Low
#'
#' @return The compressed image
#' @export
#'
#' @examples
#' ImgCompress(img, method="resize", level=1)
ImgCompress <- function(img, method, level=1){
  TRUE
}
