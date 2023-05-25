#' Save Data
#'
#' text here.
#'

save_data<-function(img_num=y){
  Index_blob<-paste0(sub(".tif", replacement = "blob_", x=imgNames[[img_num]]))
  Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[img_num]]))
  newpath<-file.path(image_savdir,Index1)
  if(!dir.exists(newpath)){
    dir.create(newpath)
  }
  for(k in 1:dim(st_blob_rm)[3]) {
    st_imgs_blob<-st_blob_rm[, , k]
    analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[img_num]]),"_frame")
    analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
    analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_blob_analyzed.tiff")
    features.blob2$frame_num[k]<-cbind(k)
    writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
  }
  csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
  write.csv(features.blob2, paste0(newpath, csv_save)) #Change this CSV file name

  #grey st img save
  Index_grey<-paste0(sub(".tif", replacement = "grey_", x=imgNames[[img_num]]))
  Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[img_num]]))
  newpath<-file.path(image_savdir,Index1)
  if(!dir.exists(newpath)){
    dir.create(newpath)
  }
  for(k in 1:dim(st_img_rm)[3]) {
    st_imgs_grey<-st_img_rm[, , k]
    analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[img_num]]),"_frame")
    analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
    analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_grey_analyzed.tiff")
    features.img2$frame_num[k]<-cbind(k)
    writeImage(st_imgs_grey,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
  }
  csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
  write.csv(features.img2, paste0(newpath, csv_save)) #Change this CSV file name
}
