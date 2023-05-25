#' Cell segmentation
#'
#' Cell segmentation function of uploaded and converted images. Users can adjust
#' the provided variables to improve cell segementation.
#'

cell_segment<-function(img_num=y,img_w=1280,img_h=1024,lambda_adj=10^1){
  seed.input<-lapply(seed.input,as.numeric)
  seed.input<-as.data.frame(seed.input)
  seed.input<-round(seed.input,0)
  seed.mtx<-circle_matrix(img_w,img_h,c(seed.input[1,1]),c(seed.input[1,2]),5,f=1)

  for (j in 2:nrow(seed.input)){
    S1.mtx<-circle_matrix(img_w,img_h,c(seed.input[j,1]),c(seed.input[j,2]),5,f=1)
    point.select<-which(S1.mtx==1,arr.ind = TRUE)
    point.select<-as.data.frame(point.select)
    seed.mtx[ as.matrix(point.select) ] <- 1
  }

  seed.mtx<-seed.mtx[,c(img_h:1),drop = FALSE]

  display(seed.mtx)

  seed.mtx.img<-Image(seed.mtx)
  display(seed.mtx.img)
  seed_img<-single_cell_convert(seed.mtx.img)
  display(seed_img)

  ctmask<-opening(img_watershed>0.1,makeBrush(5,shape='disc'))
  cmask<-propagate(neg_imgs[[img_num]],seeds=seed_img,mask=ctmask,lambda = lambda_adj)
  display(cmask)
  segmented<-paintObjects(cmask,grey_imgs[[img_num]],col = c('pink','red'))
  display(segmented,all=TRUE)
  st_blob <- stackObjects(cmask,img_watershed)
  st_img <- stackObjects(cmask,grey_imgs[[img_num]])
  st_img_test<-Image(st_img)
  return(st_blob)
  return(st_img)
  return(st_img_test)
}
