library(EBImage)
library(tidyverse)
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
library(DT)
library(cyanocount2.0)

####START ANALYSIS####
# clear environment and free unused memory
gc()

#Change directories/Import images
img_dir <- ("D:/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F271/40X/Raw_Imgs/Batch_1")
orient_savdir <- ("D:/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F271/40X/Raw_Imgs")
folder_name<-basename(img_dir)

Index1<-paste0("/Orient_1_",folder_name,"/")
newpath1<-file.path(orient_savdir,Index1,"")
if(!dir.exists(newpath1)){
  dir.create(newpath1)
}
Index2<-paste0("/Orient_2_",folder_name,"/")
newpath2<-file.path(orient_savdir,Index2,"")
if(!dir.exists(newpath2)){
  dir.create(newpath2)
}
Index3<-paste0("/Orient_3_",folder_name,"/")
newpath3<-file.path(orient_savdir,Index3,"")
if(!dir.exists(newpath3)){
  dir.create(newpath3)
}

images <- list.files(img_dir, pattern = NULL, full.name = F)

if(grepl("(?i).jpg", images[[2]])==TRUE){
  images <- list.files(img_dir, pattern = "jpg", full.name = T)
  images_names <- list.files(img_dir, pattern = "jpg", full.name = F)
  imgNames <- paste0(images_names)
  read_images <- lapply(images, image_read)
  names(images) <- imgNames
} else if (grepl("(?i).tif", images[[2]])==TRUE){
  images <- list.files(img_dir, pattern = "tif", full.name = T)
  images_names <- list.files(img_dir, pattern = "tif", full.name = F)
  imgNames <- paste0(images_names)
  read_images <- lapply(images, image_read)
  names(images) <- imgNames
}

if(grepl("(?i).jpg", images_names[[1]])==TRUE){
  for (j in 1:length(read_images)){
    img_flip<-image_flip(read_images[[j]])
    analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[j]]),"_2.jpg")
    image_write(img_flip,path = paste0(newpath1, analyzed_image1))
  }
  for (j in 1:length(read_images)){
    img_flop<-image_flop(read_images[[j]])
    analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[j]]),"_3.jpg")
    image_write(img_flop,path = paste0(newpath2, analyzed_image1))
  }
  for (j in 1:length(read_images)){
    img_flop<-image_flop(read_images[[j]])
    img_flip<-image_flip(img_flop)
    analyzed_image1<-paste0(sub(".jpg", replacement = "", x=imgNames[[j]]),"_4.jpg")
    image_write(img_flip,path = paste0(newpath3, analyzed_image1))
  }
} else if (grepl("(?i).tif", images_names[[1]])==TRUE){
  for (j in 1:length(read_images)){
    img_flip<-image_flip(read_images[[j]])
    analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[j]]),"_2.tif")
    image_write(img_flip,path = paste0(newpath1, analyzed_image1))
  }
  for (j in 1:length(read_images)){
    img_flop<-image_flop(read_images[[j]])
    analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[j]]),"_3.tif")
    image_write(img_flop,path = paste0(newpath2, analyzed_image1))
  }
  for (j in 1:length(read_images)){
    img_flop<-image_flop(read_images[[j]])
    img_flip<-image_flip(img_flop)
    analyzed_image1<-paste0(sub(".tif", replacement = "", x=imgNames[[j]]),"_4.tif")
    image_write(img_flip,path = paste0(newpath3, analyzed_image1))
  }
}
