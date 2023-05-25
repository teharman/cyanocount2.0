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
library(DT)

# clear environment and free unused memory
gc()

#Change directories/Import images
img_dir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/version_2/5_4_demo/NZ_Anabena/40x_AccuScope/")
image_savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/version_2/5_4_demo/NZ_Anabena/40x_AccuScope")
images <- list.files(img_dir, pattern = "tif", full.name = T)
images_names <- list.files(img_dir, pattern = "tif", full.name = F)

imgNames <- paste0(images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

#img number
y<-3

#Initial image conversion
img_process()

#Shiny UI cell selector
point_select()

#Seed segmentation
cell_segment()

#Shiny UI to remove certain cells
image_select()

#Removal of problematic images from output of 'image_select' Shiny UI
st_blob_rm<-st_blob[,,-image_num2]
st_img_rm<-st_img[,,-image_num2]

#Create features and export images
get_features()

#Data save
save_data()
