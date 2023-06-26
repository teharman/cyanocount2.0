library(reticulate)
library(keras)
library(tensorflow)
library(tidyverse)

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model")

model_label<-dir("Train/")
output_n<-length(model_label)
save(model_label, file="label_list.R")

width<-100
height<-100
target_size<-c(width,height)
rgb<-3

path_train<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/Train/"
train_data_gen<-image_data_generator(rescale=1/255,
                                     validation_split = 0.2)
train_images<-flow_images_from_directory(path_train,
                                         train_data_gen,
                                         subset="training",
                                         target_size=target_size,
                                         class_mode = "categorical",
                                         shuffle = F,
                                         classes = model_label,
                                         seed = 2021)

table(train_images$classes)
plot(as.raster(train_images[[1]][[1]][123,,,]))
