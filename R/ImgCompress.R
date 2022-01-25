library(jpeg)

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
#' img <- array(runif(1000),dim=c(10,10,3))
#' ImgCompress(img, method="resize", level=1)
#'
ImgCompress <- function(img, method, level=1){
  if (!is.array(img)) {
    stop("Check input image type, should be an array type. Use jpeg::readJPEG('image.jpeg').")
  }
  if (nrow(img) <= 0 | ncol(img) <= 0) {
    stop("Invalid input image size.")
  }
  if (!(tolower(method) %in% c("svd","resize"))) {
    stop("Invalid compression method, expecting 'svd' or 'resize'.")
  }
  if (!(level %in% 1:3) | !is.numeric(level)) {
    stop("Invalid compression level, expecting 1, 2 or 3.")
  }

  h <- nrow(img)
  w <- ncol(img)

  if (tolower(method)=="svd"){
    level_scale = c(0.05, 0.3, 0.9)
    img_comp <- array(rep(0,length(img)),dim = dim(img))

    for (c in 1:dim(img)[3]) {
      img_svd <- svd(img[,,c])
      x <- floor(length(img_svd$d)*max(level_scale[level],1))
      img_comp[,,c] <- img_svd$u[,1:x] %*% diag(img_svd$d[1:x]) %*% t(img_svd$v[,1:x])
      img_comp[,,c] <- ifelse(img_comp[,,c] > 1,1,ifelse(img_comp[,,c] < 0,0,img_comp[,,c]))
    }

    return(img_comp)
  } else if (tolower(method)=="resize") {
    level_scale = c(0.3, 0.5, 0.7)

    h_new <- floor(h*level_scale[level])
    w_new <- floor(w*level_scale[level])

    img_comp <- array(rep(0,h_new*w_new*dim(img)[3]),dim = c(h_new, w_new, dim(img)[3]))

    for (c in 1:dim(img)[3]) {
      for (i in 1:(w_new)) {
        for (j in 1:(h_new)) {
          img_comp[i, j, c] <- img[floor(i/level_scale[level]), floor(j/level_scale[level]), c]
        }
      }
    }

    return(img_comp)
  }
}




#
#
# test_that("Error should be thrown when input is not array type", {
#   expect_error(ImgCompress(c(1,2,3),method="svd",level=1))
# })
#
# test_that("Error should be thrown when input image size is not valid", {
#   expect_error(ImgCompress(array(runif(1000),dim=c(0,0,3)),method="svd",level=1))
# })
#
# test_that("Error should be thrown when input method is not expected", {
#   expect_error(ImgCompress(array(runif(1000),dim=c(10,10,3)),method="apple",level=1))
# })
#
# test_that("Error should be thrown when input level is not expected", {
#   expect_error(ImgCompress(array(runif(1000),dim=c(10,10,3)),method="svd",level=5))
# })
#
# test_that("Validate function svd method output is correct", {
#   expect_equal(ImgCompress(array(1000,dim=c(10,10,3)),method="svd",level=1), array(rep(1,1000),dim=c(10,10,3)))
# })
#
# test_that("Validate function resize output is correct", {
#   expect_equal(dim(ImgCompress(array(1000,dim=c(10,10,3)),method="resize",level=2)), c(5, 5, 3))
# })
#
#
#
# level <- 2
#
# img <- readJPEG('C:/MDS/Block 4/DSCI524/labs/group/ImgHelpR/test_img/ubc.jpeg')
# img_comp <- ImgCompress(img,method="svd",level)
# img_comp2 <- ImgCompress(img,method="resize",level)
#
# h <- nrow(img_comp2)
# w <- ncol(img_comp2)
#
# plot(1:max(h, w), type='n')
# rasterImage(img_comp2, 1, 1, w, h)

#writeJPEG(img_comp,'C:/MDS/Block 4/DSCI524/labs/group/ImgHelpR/test_img/ubc_comp.jpeg')
