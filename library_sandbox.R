library(EBImage)
library(tiff)
library(png)
library(pixmap)
library(raster)
library(cellcount)
library(dplyr)
library(jjb)
library(shiny)
library(magick)
library(ggplot2)
library(shinyalert)
library(shinyFiles)
library(shinyjs)
library(cowplot)
library(DT)
library(leaflet)
library(imager)
library(cyanocount2.0)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyWidgets)
library(DiagrammeR)
library(magrittr)

greyscale_convert <- function(x, contrast = 1,brightness=0.1,increase=TRUE) {
  if(increase == 'TRUE'){
    x1 <- x * contrast
    x2 <- x1 + brightness
    x3 <- x2[, , 1] + x2[, , 2] + x2[, , 3]
    x4 <- x3 / max(x3)
    x5 <- normalize(x4, inputRange = c(0.1, 0.75)
                    )
    return(x5)
  }else{
    x1 <- x * contrast
    x2 <- x1 - brightness
    x3 <- x2[, , 1] + x2[, , 2] + x2[, , 3]
    x4 <- x3 / max(x3)
    x5 <- normalize(x4, inputRange = c(0.1,0.75)
                    )
    return(x5)
  }
}
binary<-function(x, adj = 0.5) {
  binary_img<- x > adj
  return(binary_img)
}
img_neg<-function(x) {
  imgneg<-max(x)-x
  return(imgneg)
}
watershed_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1, removeEdgeCells = TRUE) {
  if (removeEdgeCells == TRUE){
    image <- thresh(x, w = w, h = h, offset = offset)
    image1 <- fillHull(image)
    image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
    nf <- computeFeatures.shape(image2)
    nr <- which(nf[, "s.area"] < areathresh)
    image3 <- rmObjects(image2, nr)
    dims <- dim(image3)
    border1 <- c(image3[1:dims[1], 1], image3[1:dims[1], dims[2]], image3[1, 1:dims[2]], image3[dims[1], 1:dims[2]])
    ids <- unique(border1[which(border1 != 0)])
    inner <- rmObjects(image3, ids)
    return(inner)
  } else{
    image <- thresh(x, w = w, h = h, offset = offset)
    image1 <- fillHull(image)
    image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
    nf <- computeFeatures.shape(image2)
    nr <- which(nf[, "s.area"] < areathresh)
    image3 <- rmObjects(image2, nr)
    return(image3)
  }
}
single_cell_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
  image <- thresh(x, w = w, h = h, offset = offset)
  image1 <- fillHull(image)
  image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
  nf <- computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < areathresh)
  image3 <- rmObjects(image2, nr)
  return(image3)
}
#______________________________________________________________________________#


####START ANALYSIS####
# clear environment and free unused memory
gc()

#Change directories/Import images
img_dir <- ("D:/CyanoSCOPE_imgs/AccuScope/Edenton_Anabaena/20X/Raw_Imgs/Orient_1_Batch_6/")
image_savdir <- ("D:/CyanoSCOPE_imgs/AccuScope/Edenton_Anabaena/20X/Raw_Imgs/Orient_1_Batch_6/")
mask_savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Edenton_Anabaena/20X/True_Mask/")

images <- list.files(img_dir, pattern = NULL, full.name = F)

if(grepl("(?i).jpg", images[[2]])==TRUE){
  images <- list.files(img_dir, pattern = "jpg", full.name = T)
  images_names <- list.files(img_dir, pattern = "jpg", full.name = F)
  imgNames <- paste0(images_names)
  read_images <- lapply(images, jpeg::readJPEG)
  img_transposed <- lapply(read_images,aperm,c(2,1,3))
  names(images) <- imgNames
} else if (grepl("(?i).tiff", images[[2]])==TRUE){
  images <- list.files(img_dir, pattern = "tif", full.name = T)
  images_names <- list.files(img_dir, pattern = "tif", full.name = F)
  imgNames <- paste0(images_names)
  read_images <- lapply(images, tiff::readTIFF)
  img_transposed <- lapply(read_images,aperm,c(2,1,3))
  names(images) <- imgNames
}

#img number
y<-7
dim(img_transposed[[y]])
height<-dim(img_transposed[[y]])[2]
height<-as.numeric(height)
width<-dim(img_transposed[[y]])[1]
width<-as.numeric(width)

#Separate blue channel from inp7t images
rgb.imgs<-Image(img_transposed[[y]],colormode = Color)
EBImage::display(rgb.imgs)

####Initial image conversion####
grey_imgs<-lapply(img_transposed, greyscale_convert, contrast = 4, brightness = 1.5, increase=FALSE)
EBImage::display(grey_imgs[[y]])
neg_imgs<-lapply(grey_imgs, img_neg)
EBImage::display(neg_imgs[[y]])
binary_img<-lapply(neg_imgs, binary, adj = 0.65)
EBImage::display(binary_img[[y]])
imagesMapped <- lapply(binary_img, mapped, threshold = 0.1) #background intensity threshold adjustment
img_watershed<-watershed_convert(imagesMapped[[y]],w=25,h=25,offset=0.001,areathresh=100,tolerance = 0.6,ext = 4,removeEdgeCells=TRUE)
EBImage::display(img_watershed)

#Shiny UI cell selector
point_select()

#Seed segmentation
seed.input<-lapply(seed.input,as.numeric)
seed.input<-as.data.frame(seed.input)
seed.input<-round(seed.input,0)
seed.mtx<-circle_matrix(width,height,c(seed.input[1,1]),c(seed.input[1,2]),5,f=1)
for (j in 2:nrow(seed.input)){
  S1.mtx<-circle_matrix(width,height,c(seed.input[j,1]),c(seed.input[j,2]),5,f=1)
  point.select<-which(S1.mtx==1,arr.ind = TRUE)
  point.select<-as.data.frame(point.select)
  seed.mtx[ as.matrix(point.select) ] <- 1
}
seed.mtx<-seed.mtx[,c(height:1),drop = FALSE]
seed.mtx.img<-Image(seed.mtx)
seed_img<-single_cell_convert(seed.mtx.img)

ctmask<-opening(img_watershed>0.1,makeBrush(5,shape='disc'))
cmask<-propagate(neg_imgs[[y]],seeds=seed_img,mask=ctmask,lambda = 10^1)
EBImage::display(cmask)
mask_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[y]]),"_mask.png")
writeImage(cmask,files = paste0(mask_savdir, mask_image1))
segmented<-paintObjects(cmask,rgb.imgs,col = c('black','orange'))
EBImage::display(segmented,all=TRUE)
st_blob <- stackObjects(cmask,img_watershed)
st_img <- stackObjects(cmask,rgb.imgs)
st_img_test<-Image(st_img)

#Shiny UI to remove certain cells
image_select()

#select one
remove_images<-c('TRUE')
remove_images<-c('FALSE')

####save cell images####

if(grepl("(?i).jpg", imgNames[[y]])==TRUE){
  if(remove_images == 'TRUE'){
    #Removal of problematic images from output of 'image_select' Shiny UI
    st_blob_rm<-st_blob[,,-image_num2]
    st_img_rm<-st_img[,,,-image_num2]

    ####Create features and export images####
    features.data.blob<-computeFeatures(cmask,img_watershed)
    features.data.img<-computeFeatures(cmask,rgb.imgs)
    features.blob<-as.data.frame(features.data.blob)
    features.img<-as.data.frame(features.data.img)
    features.blob1<-cbind(features.blob,frame_num=NA)
    features.img1<-cbind(features.blob,frame_num=NA)

    #removal of rows from the shiny_select UI
    features.blob2<-features.blob1 %>%  filter(!row_number() %in% image_num2)
    features.img2<-features.img1 %>% filter(!row_number() %in% image_num2)

    #blob st img save
    Index_blob<-paste0(sub(".jpg", replacement = "blob_", x=imgNames[[y]]))
    Index1<-paste0(sub(".jpg", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_blob_rm)[3]) {
      st_imgs_blob<-st_blob_rm[, , k]
      analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[y]]),"_blob_frame (")
      analyzed_image2<-paste0(sub(".jpg", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".jpg", replacement = "", x=analyzed_image2),").tiff")
      features.blob2$frame_num[k]<-cbind(k)
      writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
    write.csv(features.blob2, paste0(newpath, csv_save)) #Change this CSV file name

    #grey st img save
    Index_grey<-paste0(sub(".jpg", replacement = "color_", x=imgNames[[y]]))
    Index1<-paste0(sub(".jpg", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_img_rm)[4]) {
      st_imgs_color<-st_img_rm[, , , k]
      analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[y]]),"_color_frame (")
      analyzed_image2<-paste0(sub(".jpg", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".jpg", replacement = "", x=analyzed_image2),").tiff")
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
    Index_blob<-paste0(sub(".jpg", replacement = "blob_", x=imgNames[[y]]))
    Index1<-paste0(sub(".jpg", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_blob)[3]) {
      st_imgs_blob<-st_blob[, , k]
      analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[y]]),"_blob_frame (")
      analyzed_image2<-paste0(sub(".jpg", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".jpg", replacement = "", x=analyzed_image2),").tiff")
      features.blob1$frame_num[k]<-cbind(k)
      writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
    write.csv(features.blob1, paste0(newpath, csv_save)) #Change this CSV file name

    #grey st img save
    Index_grey<-paste0(sub(".jpg", replacement = "color_", x=imgNames[[y]]))
    Index1<-paste0(sub(".jpg", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_img)[4]) {
      st_imgs_color<-st_img[, , , k]
      analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[y]]),"_color_frame (")
      analyzed_image2<-paste0(sub(".jpg", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".jpg", replacement = "", x=analyzed_image2),").tiff")
      features.img1$frame_num[k]<-cbind(k)
      writeImage(st_imgs_color,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
    write.csv(features.img1, paste0(newpath, csv_save)) #Change this CSV file name
  }
} else if(grepl("(?i).tiff", imgNames[[y]])==TRUE){
  if(remove_images == 'TRUE'){
    #Removal of problematic images from output of 'image_select' Shiny UI
    st_blob_rm<-st_blob[,,-image_num2]
    st_img_rm<-st_img[,,,-image_num2]

    ####Create features and export images####
    features.data.blob<-computeFeatures(cmask,img_watershed)
    features.data.img<-computeFeatures(cmask,rgb.imgs)
    features.blob<-as.data.frame(features.data.blob)
    features.img<-as.data.frame(features.data.img)
    features.blob1<-cbind(features.blob,frame_num=NA)
    features.img1<-cbind(features.blob,frame_num=NA)

    #removal of rows from the shiny_select UI
    features.blob2<-features.blob1 %>%  filter(!row_number() %in% image_num2)
    features.img2<-features.img1 %>% filter(!row_number() %in% image_num2)

    #blob st img save
    Index_blob<-paste0(sub(".tif", replacement = "blob_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_blob_rm)[3]) {
      st_imgs_blob<-st_blob_rm[, , k]
      analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[y]]),"_blob_frame (")
      analyzed_image2<-paste0(sub(".tif", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = "", x=analyzed_image2),").tiff")
      features.blob2$frame_num[k]<-cbind(k)
      writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
    write.csv(features.blob2, paste0(newpath, csv_save)) #Change this CSV file name

    #grey st img save
    Index_grey<-paste0(sub(".tif", replacement = "color_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_img_rm)[4]) {
      st_imgs_color<-st_img_rm[, , , k]
      analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[y]]),"_color_frame (")
      analyzed_image2<-paste0(sub(".tif", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = "", x=analyzed_image2),").tiff")
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
    Index1<-paste0(sub(".tif", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_blob)[3]) {
      st_imgs_blob<-st_blob[, , k]
      analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[y]]),"_blob_frame (")
      analyzed_image2<-paste0(sub(".tif", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = "", x=analyzed_image2),").tiff")
      features.blob1$frame_num[k]<-cbind(k)
      writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
    write.csv(features.blob1, paste0(newpath, csv_save)) #Change this CSV file name

    #grey st img save
    Index_grey<-paste0(sub(".tif", replacement = "color_", x=imgNames[[y]]))
    Index1<-paste0(sub(".tif", replacement = "/", x=imgNames[[y]]))
    newpath<-file.path(image_savdir,Index1,"")
    if(!dir.exists(newpath)){
      dir.create(newpath)
    }
    for(k in 1:dim(st_img)[4]) {
      st_imgs_color<-st_img[, , , k]
      analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[y]]),"_color_frame (")
      analyzed_image2<-paste0(sub(".tif", replacement = "", x=analyzed_image1),k)
      analyzed_image3<-paste0(sub(".tif", replacement = "", x=analyzed_image2),").tiff")
      features.img1$frame_num[k]<-cbind(k)
      writeImage(st_imgs_color,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
    }
    csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
    write.csv(features.img1, paste0(newpath, csv_save)) #Change this CSV file name
  }
}
