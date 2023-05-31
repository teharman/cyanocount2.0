#' Save Data
#'
#' text here.
#'

save_data<-function(img_num=y,remove_images='TRUE'){
  if(remove_images == 'TRUE'){
    #Removal of problematic images from output of 'image_select' Shiny UI
    st_blob_rm<-st_blob[,,-image_num2]
    st_img_rm<-st_img[,,,-image_num2]

    ####Create features and export images####
    features.data.blob<-computeFeatures(cmask,img_watershed)
    features.data.img<-computeFeatures(cmask,grey_imgs[[y]])
    features.blob<-as.data.frame(features.data.blob)
    features.img<-as.data.frame(features.data.img)
    features.blob1<-cbind(features.blob,frame_num=NA)
    features.img1<-cbind(features.blob,frame_num=NA)

    #removal of rows from the shiny_select UI
    features.blob2<-features.blob1 %>%  filter(!row_number() %in% image_num2)
    features.img2<-features.img1 %>% filter(!row_number() %in% image_num2)

    #blob st img save
    Index_blob<-paste0(sub(".tif", replacement = "blob_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1)
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_blob_rm)[3]) {
      st_imgs_blob<-st_blob_rm[, , k]
      analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[y]]),"_frame")
      analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_blob_analyzed.tiff")
      features.blob2$frame_num[k]<-cbind(k)
      writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
    write.csv(features.blob2, paste0(newpath, csv_save)) #Change this CSV file name

    #grey st img save
    Index_grey<-paste0(sub(".tif", replacement = "grey_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1)
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_img_rm)[4]) {
      st_imgs_color<-st_img_rm[, , , k]
      analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[y]]),"_frame")
      analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_grey_analyzed.tiff")
      features.img2$frame_num[k]<-cbind(k)
      writeImage(st_imgs_color,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
    write.csv(features.img2, paste0(newpath, csv_save)) #Change this CSV file name
  } else{
    ####Create features and export images####
    features.data.blob<-computeFeatures(cmask,img_watershed)
    features.data.img<-computeFeatures(cmask,grey_imgs[[y]])
    features.blob<-as.data.frame(features.data.blob)
    features.img<-as.data.frame(features.data.img)
    features.blob1<-cbind(features.blob,frame_num=NA)
    features.img1<-cbind(features.blob,frame_num=NA)

    #blob st img save
    Index_blob<-paste0(sub(".tif", replacement = "blob_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1)
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_blob)[3]) {
      st_imgs_blob<-st_blob[, , k]
      analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[y]]),"_frame")
      analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_blob_analyzed.tiff")
      features.blob1$frame_num[k]<-cbind(k)
      writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
    write.csv(features.blob1, paste0(newpath, csv_save)) #Change this CSV file name

    #grey st img save
    Index_grey<-paste0(sub(".tif", replacement = "grey_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1)
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_img)[4]) {
      st_imgs_color<-st_img[, , , k]
      analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[y]]),"_frame")
      analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_grey_analyzed.tiff")
      features.img1$frame_num[k]<-cbind(k)
      writeImage(st_imgs_color,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
    write.csv(features.img1, paste0(newpath, csv_save)) #Change this CSV file name
  }
}
