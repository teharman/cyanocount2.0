Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
library(reticulate)
py_config()
reticulate::py_install('pillow')

library(keras)
library(tensorflow)
library(tidyverse)
#__________________________________________________________________________

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

batch_size<-32
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

test_image <- image_load("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/Test/Anabaena/Anabaena_20X_color_test (1).tif",
                         target_size = target_size)

x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred
