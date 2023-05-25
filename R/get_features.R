#' Get Features
#'
#' text here.
#'

get_features<-function(img_num=y){
  features.data.blob<-computeFeatures(cmask,img_watershed)
  features.data.img<-computeFeatures(cmask,grey_imgs[[img_num]])
  features.blob<-as.data.frame(features.data.blob)
  features.img<-as.data.frame(features.data.img)
  features.blob1<-cbind(features.blob,frame_num=NA)
  features.img1<-cbind(features.blob,frame_num=NA)

  #removal of rows from the shiny_select UI
  features.blob2<-features.blob1 %>%  filter(!row_number() %in% image_num2)
  features.img2<-features.img1 %>% filter(!row_number() %in% image_num2)
  return(features.blob2)
  return(features.img2)
}
