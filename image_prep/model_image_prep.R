library(reticulate)
library(keras)
library(tensorflow)
library(tidyverse)
library(fs)
library(EBImage)
library(magick)
library(tidyr)
library(tidyverse)
library(dplyr)
#__________________________________________________________________________

#Write from TIFF to PNG - save to main image folder

cell_tif <- ('D:/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F271/20X/Raw_Imgs/Batch_1/F271_AS_20X_Image001/')
blob_images <- list.files(cell_tif, pattern = "blob_analyzed.tiff", full.name = T)
blob_names <- list.files(cell_tif, pattern = "blob_analyzed.tiff", full.name = F)
color_images <- list.files(cell_tif, pattern = "color_analyzed.tiff", full.name = T)
color_names <- list.files(cell_tif, pattern = "color_analyzed.tiff", full.name = F)
blob_read <- lapply(blob_images, readTIFF)
color_read <- lapply(color_images, readTIFF)

blob_dir <- ('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F271/20X/Total_Blob/')
color_dir <- ('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F271/20X/Total_Color/')

for(u in 1:length(blob_images)){
  cell_name <- blob_names[[u]]
  writeImage(blob_read[[u]],files = paste0(blob_dir,cell_name))
}

for(u in 1:length(color_images)){
  cell_name <- color_names[[u]]
  rgb.img<-Image(color_read[[u]],colormode = Color)
  writeImage(rgb.img,files = paste0(color_dir,cell_name))
}
