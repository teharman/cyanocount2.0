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
img_dir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Microcystis_F192/20X/Raw_Imgs/Batch_6")
orient_savdir1 <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Microcystis_F192/20X/Raw_Imgs/Batch_22/")
orient_savdir2 <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Microcystis_F192/20X/Raw_Imgs/Batch_23/")
orient_savdir3 <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Microcystis_F192/20X/Raw_Imgs/Batch_24/")
images <- list.files(img_dir, pattern = "tif", full.name = T)
images_names <- list.files(img_dir, pattern = "tif", full.name = F)

imgNames <- paste0(images_names)
read_images <- lapply(images, image_read)
names(images) <- imgNames

for (j in 1:length(read_images)){
  img_flip<-image_flip(read_images[[j]])
  analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[j]]),"_2.tif")
  image_write(img_flip,path = paste0(orient_savdir1, analyzed_image1))
}

for (j in 1:length(read_images)){
  img_flop<-image_flop(read_images[[j]])
  analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[j]]),"_3.tif")
  image_write(img_flop,path = paste0(orient_savdir2, analyzed_image1))
}

for (j in 1:length(read_images)){
  img_flop<-image_flop(read_images[[j]])
  img_flip<-image_flip(img_flop)
  analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[j]]),"_2.tif")
  image_write(img_flip,path = paste0(orient_savdir3, analyzed_image1))
}

