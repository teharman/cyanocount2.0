Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
library(reticulate)
py_config()
reticulate::py_install('pillow')

library(keras)
library(tensorflow)
library(tidyverse)
library(fs)
#__________________________________________________________________________
####old model####
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
validation_images <- flow_images_from_directory(path_train,
                                                train_data_gen,
                                                subset = 'validation',
                                                target_size = target_size,
                                                class_mode = "categorical",
                                                classes = model_label,
                                                seed = 2021)

table(train_images$classes)
plot(as.raster(train_images[[1]][[1]][12,,,]))

mod_base <- application_xception(weights = 'imagenet',
                                 include_top = FALSE, input_shape = c(width, height, 3))
freeze_weights(mod_base)

model_function <- function(learning_rate = 0.001,
                           dropoutrate=0.2, n_dense=1024){

  k_clear_session()

  model <- keras_model_sequential() %>%
    mod_base %>%
    layer_global_average_pooling_2d() %>%
    layer_dense(units = n_dense) %>%
    layer_activation("relu") %>%
    layer_dropout(dropoutrate) %>%
    layer_dense(units=output_n, activation="softmax")

  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learning_rate),
    metrics = "accuracy"
  )

  return(model)

}

model<-model_function()
model

batch_size<-128
epochs<-6

hist <- model %>% fit_generator(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size,
  epochs = epochs,
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 2
)

path_test<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/Test/"

test_data_gen <- image_data_generator(rescale = 1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021)
model %>% evaluate_generator(test_images,
                             steps = test_images$n)

test_image <- image_load("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/Test/Microcystis/Microcystis_40X_color_test (5).tif",
                         target_size = target_size)

x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred


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
original_dir<-path("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/test_folder")
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

test_model<-load_model_tf("convnet_from_scratch.keras")
result<-evaluate(test_model,test_dataset)
