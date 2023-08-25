Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
py_config()
reticulate::py_install('pillow')

library(reticulate)
library(keras)
library(tensorflow)
library(tidyverse)
library(fs)
#__________________________________________________________________________

####new model####


inputs<-layer_input(shape=c(100,100,3))
outputs<-inputs%>%
  layer_rescaling(1/255)%>%
  layer_conv_2d(filters=32,kernel_size = 3, activation = "relu")%>%
  layer_max_pooling_2d(pool_size = 2)%>%
  layer_conv_2d(filters=64,kernel_size = 3, activation = "relu")%>%
  layer_max_pooling_2d(pool_size = 2)%>%
  layer_conv_2d(filters=128,kernel_size = 3, activation = "relu")%>%
  layer_max_pooling_2d(pool_size = 2)%>%
  layer_conv_2d(filters=256,kernel_size = 3, activation = "relu")%>%
  layer_max_pooling_2d(pool_size = 2)%>%
  layer_conv_2d(filters=256,kernel_size = 3, activation = "relu")%>%
  layer_flatten()%>%
  layer_dense(1, activation = "sigmoid")
new.model<-keras_model(inputs,outputs)
new.model

new.model%>%compile(loss="binary_crossentropy",
                    optimizer = "rmsprop",
                    metrics = "accuracy")

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model")
original_dir<-path("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/data/test_folder")
new_base_dir<-path("small_data")

make_subset_20X<-function(subset_name, start_index, end_index){
  for (category in c("Anabaena", "F192")){
    file_name<-glue::glue("{category}_20X_color ({start_index:end_index}).png")
    dir_create(new_base_dir / subset_name / category)
    file_copy(original_dir / file_name ,
              new_base_dir / subset_name / category / file_name)
  }
}

make_subset_40X<-function(subset_name, start_index, end_index){
  for (category in c("Anabaena", "F192")){
    file_name<-glue::glue("{category}_40X_color ({start_index:end_index}).png")
    dir_create(new_base_dir / subset_name / category)
    file_copy(original_dir / file_name ,
              new_base_dir / subset_name / category / file_name)
  }
}

make_subset_20X("train", start_index = 1, end_index = 250)
make_subset_40X("train", start_index = 1, end_index = 250)

make_subset_20X("validation", start_index = 251, end_index = 375)
make_subset_40X("validation", start_index = 251, end_index = 375)

make_subset_20X("test", start_index = 376, end_index = 625)
make_subset_40X("test", start_index = 376, end_index = 625)

train_dataset<-image_dataset_from_directory(new_base_dir / "train",
                                            image_size = c(100,100),
                                            batch_size = 32)
validation_dataset<-image_dataset_from_directory(new_base_dir / "validation",
                                            image_size = c(100,100),
                                            batch_size = 32)
test_dataset<-image_dataset_from_directory(new_base_dir / "test",
                                            image_size = c(100,100),
                                            batch_size = 32)

c(data_batch,labels_batch) %<-% iter_next(as_iterator(train_dataset))
data_batch$shape
labels_batch$shape

callbacks <- list(
  callback_model_checkpoint(
    filepath = "convnet_from_scratch.keras",
    save_best_only = TRUE,
    monitor = "val_loss"
  )
)

history <- new.model %>%
  fit(
    train_dataset,
    epochs = 30,
    validation_data = validation_dataset,
    callbacks = callbacks
  )

save_model_tf(new.model, "small_data/model/") #save model here
test_model<-load_model_tf('small_data/model/', custom_objects = NULL, compile = TRUE)
result<-evaluate(test_model,test_dataset)

model_label<-dir("Train/")
test_image <- image_load("misc/Test/Microcystis/Microcystis_20X_color_test (9).tif",
                         target_size = c(100,100))
x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- test_model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred
