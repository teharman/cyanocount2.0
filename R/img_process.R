#' Image Process
#'
#' Initial conversion of uploaded images using tools from the *cellcount* package.
#' Users can adjust various parameters to meet the needs that the image requires for optimal
#' cell segmentation.
#'

img_process<-function(img_num=1,contrast_adj=0.2,bright_adj=0.1,increase=TRUE,thresh_adj=0.28,w=50,h=50,offset=0.001,area_thresh=250,tolerance=0.8,ext=3){
  greyscale <- function(x, contrast = 1,
                        brightness=0.1,increaseTF=TRUE) {
    if(increase == 'TRUE'){
      x <- x * contrast
      x <- x + brightness
      x <- x[, , 1] + x[, , 2] + x[, , 3]
      x <- x / max(x)
      x <- normalize(x, inputRange = c(0.1, 0.75))
      return(x)
    }else{
      x <- x * contrast
      x <- x - brightness
      x <- x[, , 1] + x[, , 2] + x[, , 3]
      x <- x / max(x)
      x <- normalize(x, inputRange = c(0.1, 0.75))
      return(x)
    }
  }
  grey_imgs<-lapply(img_transposed, greyscale, contrast = contrast_adj,brightness=bright_adj,increaseTF=increase)
  display(grey_imgs[[img_num]])

  binary<-function(x, adj = 0.5) {
    binary_img<- x > adj
    return(binary_img)
  }
  img_neg<-function(x) {
    imgneg<-max(x)-x
    return(imgneg)
  }
  neg_imgs<-lapply(grey_imgs, img_neg)
  binary_img<-lapply(neg_imgs, binary, adj = thresh_adj)
  imagesMapped <- lapply(binary_img, mapped, threshold = 0.2) #background intensity threshold adjustment
  img_watershed<-single_cell_convert(imagesMapped[[img_num]],w=w,h=h,offset=offset,areathresh=area_thresh,tolerance=tolerance,ext=ext)
  return(img_watershed)
  display(img_watershed)
}
