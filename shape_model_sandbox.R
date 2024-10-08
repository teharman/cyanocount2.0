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

#virtualenv_create('C:/Users/Tyler.Harman/AppData/Local/miniconda3/envs/new-reticulate',python=install_python())
#tensorflow::install_tensorflow(envname = "C:/Users/Tyler.Harman/AppData/Local/miniconda3/envs/new-reticulate/",version='2.10-cpu',extra_packages = c('pillow','scipy'))
#reticulate::py_install("numpy<2", envname = "C:/Users/Tyler.Harman/AppData/Local/miniconda3/envs/new-reticulate", method = "virtualenv")
Sys.setenv(RETICULATE_PYTHON="C:\\Users\\Tyler.Harman\\AppData\\Local\\miniconda3\\envs\\new-reticulate\\Scripts\\python.exe")
reticulate::py_config()
#__________________________________________________________________________

#Write from TIFF to PNG - save to main image folder

cell_tif <- ('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Edenton_Anabaena/40X/Total_Blob')
images <- list.files(cell_tif, pattern = "tif", full.name = T)
images_names <- list.files(cell_tif, pattern = "tif", full.name = F)
read_images <- lapply(images, readTIFF)
sav_dir <- ('X:/CyanoSCOPE_imgs/Draft_Model/shape_data/Species/Anabaena/40X/')

for(u in 1:length(images)){
  cell_name <- images_names[[u]]
  rgb.img<-Image(read_images[[u]],colormode = Color)
  new_cell_name<-paste0(sub(".tif", replacement = " ", x=cell_name),".png")
  writeImage(rgb.img,files = paste0(sav_dir, new_cell_name))
}

#______________________________________________________________________________#

####new model####

original_dir<-path("D:/CyanoSCOPE_imgs/Draft_Model/shape_data/All_Data")
new_base_dir<-path("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/shape_model")

make_subset_20X<-function(subset_name, start_index, end_index){
  for (category in c("F108", "F192", "F199", "F271")){
    file_name<-glue::glue("{category}_AS_20X_blob ({start_index:end_index}).png")
    dir_create(new_base_dir / subset_name / category)
    file_copy(original_dir / file_name ,
              new_base_dir / subset_name / category / file_name)
  }
}

#make_subset_40X<-function(subset_name, start_index, end_index){
#  for (category in c("EdA", "F192", "F271")){
#    file_name<-glue::glue("{category}_40x_color ({start_index:end_index}).png")
#    dir_create(new_base_dir / subset_name / category)
#    file_copy(original_dir / file_name ,
#              new_base_dir / subset_name / category / file_name)
#  }
#}

make_subset_20X("train", start_index = 1, end_index = 5500)
#make_subset_40X("train", start_index = 1, end_index = 750)

make_subset_20X("validation", start_index = 5501, end_index = 6500)
#make_subset_40X("validation", start_index = 751, end_index = 1250)

make_subset_20X("test", start_index = 6501, end_index = 7000)
#make_subset_40X("test", start_index = 1251, end_index = 2000)
#______________________________________________________________________________#
setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models")

model_label<-dir("shape_model/train/")
output_n<-length(model_label)

width<-150
height<-150
target_size<-c(width,height)
rgb<-1


path_train<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/shape_model/train"
path_valid<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/shape_model/validation"
train_data_gen<-image_data_generator(rescale=1/255,
                                     #validation_split = 0.2
)
valid_data_gen<-image_data_generator(rescale=1/255,
                                     #validation_split = 0.2
)
train_images<-flow_images_from_directory(path_train,
                                         train_data_gen,
                                         subset="training",
                                         target_size=target_size,
                                         class_mode = "categorical",
                                         shuffle = F,
                                         classes = model_label,
                                         seed = 2021,
                                         color_mode = "grayscale")
validation_images <- flow_images_from_directory(path_valid,
                                                valid_data_gen,
                                                target_size = target_size,
                                                class_mode = "categorical",
                                                classes = model_label,
                                                seed = 2021,
                                                color_mode = "grayscale")

table(train_images$classes)
plot(as.raster(train_images[[1]][[1]][25,,,]))

#mod_base <- application_xception(weights = 'imagenet',
#                                 include_top = FALSE, input_shape = c(width, height, 1))
#freeze_weights(mod_base)

model_function <- function(){

  k_clear_session()

  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu', input_shape = c(width, height, 1)) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = 'relu') %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(filters = 256, kernel_size = c(3,3), activation = 'relu') %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_dropout(rate = 0.25) %>%
    layer_flatten() %>%
    layer_dense(units = 150, activation = 'relu') %>%
    layer_dropout(rate = 0.5) %>%
    layer_dense(units = 4, activation = 'softmax')


  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = "accuracy"
  )

  return(model)

}

model<-model_function()
model

batch_size<-32
epochs<-10

hist <- model %>% fit(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size,
  epochs = epochs,
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 1
)

#______________________________________________________________________________#

path_test<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/shape_model/test"

test_data_gen <- image_data_generator(rescale = 1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021,
                                          color_mode = "grayscale")
model %>% evaluate(test_images,
                   steps = test_images$n/batch_size)

test_image <- image_load("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/shape_model/test/F192/F192_AS_20X_blob (6514).png",
                         target_size = target_size,
                         color_mode = "grayscale")

x <- image_to_array(test_image)
x <- array_reshape(x, c(1,dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred

save_model_tf(model, "shape_model/shape_model/") #save model here
model <- load_model_tf("shape_model/shape_model/")

#_______________________________________________________________________
#true vs. predicted

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models")

model_label<-dir("shape_model/train/")
output_n<-length(model_label)

width<-150
height<-150
target_size<-c(width,height)
rgb<-1

batch_size<-32
epochs<-6

path_test<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/shape_model/test"

test_data_gen <- image_data_generator(rescale = 1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021,
                                          color_mode = "grayscale")

model <- load_model_tf("shape_model/shape_model/")

classes <- test_images$classes %>%
  factor() %>%
  table() %>%
  as_tibble()
colnames(classes)[1] <- "value"

indices <- test_images$class_indices %>%
  as.data.frame() %>%
  gather() %>%
  mutate(value = as.character(value)) %>%
  left_join(classes, by = "value")

test_images$reset()

predictions <-
  predict(model,
          test_images,
          steps = as.integer(test_images$n)
  ) %>%
  round(digits = 2) %>%
  as_tibble()

colnames(predictions) <- indices$key

predictions_test <- predictions %>%
  mutate(truth_idx = as.character(test_images$classes)) %>%
  left_join(indices, by = c("truth_idx" = "value"))

pred_analysis <- predictions_test %>%
  #mutate(img_id = seq(1:test_images$n)) %>%
  mutate(img_id = seq(1:dim(predictions)[1])) %>%
  gather(pred_lbl, y, F108:F271) %>%
  group_by(img_id) %>%
  filter(y == max(y)) %>%
  arrange(img_id) %>%
  group_by(key, n, pred_lbl) %>%
  count()

pred_analysis <- pred_analysis %>%
  mutate(key = recode(key,
                      F108 = 'Microcystis_1',
                      F192 = 'Microcystis_2',
                      F199 = 'Dolichospermum_1',
                      F271 = 'Dolichospermum_2'))

pred_analysis <- pred_analysis %>%
  mutate(pred_lbl = recode(pred_lbl,
                           F108 = 'Microcystis_1',
                           F192 = 'Microcystis_2',
                           F199 = 'Dolichospermum_1',
                           F271 = 'Dolichospermum_2'))

pred_analysis_false <- pred_analysis[c(2:4,6:7,9:10),]

p <- pred_analysis %>%
  mutate(percentage_pred = nn / n * 100) %>%
  ggplot(aes(x = key, y = pred_lbl,
             fill = percentage_pred,
             label = paste0(round(percentage_pred, 2),"%"))) +
  geom_tile() +
  scale_fill_continuous() +
  scale_fill_gradient(low = "white", high = "royalblue") +
  geom_text(color = "black",size=10) +
  geom_text(data=pred_analysis_false,aes(x=key,y=pred_lbl,
                                         fill=nn/n*100,
                                         label=paste0(round(nn/n*100,2),"%")),
            color="red",size=10)+
  labs(x = "True class",
       y = "Predicted class",
       fill = "Percentage\nof \npredictions",
       #title = "True v. predicted class labels",
       #subtitle = "Percentage of test images predicted for each label"
       )+
  theme(
    plot.margin=unit(c(1,1,1,1),"cm"),
    plot.title = element_text(size=35),
    plot.subtitle = element_text(size=20),
    axis.title.x = element_text(size=25,vjust=-2),
    axis.title.y = element_text(size=25,vjust=4),
    axis.text.x = element_text(size=20),
    axis.text.y = element_text(size=20,angle=45,hjust=1,vjust=-1),
    legend.text = element_text(size=20),
    legend.title = element_text(size=20))

png("Shape_Model_Output_01.png", height = 25, width = 30, units = 'cm', res = 300)
p
dev.off()

#_______________________________________________________________________


tune_grid <- data.frame("learning_rate" = c(0.001,0.0001),
                        "dropoutrate" = c(0.3,0.2),
                        "n_dense" = c(1024,256))
tuning_results <- NULL
set.seed(2021)


for (i in 1:length(tune_grid$learning_rate)){
  for (j in 1:length(tune_grid$dropoutrate)){
    for (k in 1:length(tune_grid$n_dense)){

      model <- model_function(
        learning_rate = tune_grid$learning_rate[i],
        dropoutrate = tune_grid$dropoutrate[j],
        n_dense = tune_grid$n_dense[k])

      hist <- model %>% fit(
        train_images,
        steps_per_epoch = train_images$n %/% batch_size,
        epochs = epochs,
        validation_data = validation_images,
        validation_steps = validation_images$n %/%
          batch_size,
        verbose = 1
      )

      #Save model configurations
      tuning_results <- rbind(
        tuning_results,
        c("learning_rate" = tune_grid$learning_rate[i],
          "dropoutrate" = tune_grid$dropoutrate[j],
          "n_dense" = tune_grid$n_dense[k],
          "val_accuracy" = hist$metrics$val_accuracy))

    }
  }
}

tuning_results

best_results <- tuning_results[which(
  tuning_results[,ncol(tuning_results)] ==
    max(tuning_results[,ncol(tuning_results)])),]

best_results<-as.data.frame(best_results)

model <- model_function(learning_rate =
                          best_results$best_results[1],
                        dropoutrate = best_results$best_results[2],
                        n_dense = best_results$best_results[3])

hist <- model %>% fit(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size,
  epochs = epochs,
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 1
)

save_model_tf(model, "ID_predict_model/ID_model_test_mod/")

model <- load_model_tf("ID_predict_model/ID_model_test_mod/")

test_image <- image_load("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/test_imgs/20x_Img12/ 20x_Img12 _cell_10.tiff",
                         target_size = target_size)

x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred

classes <- test_images$classes %>%
  factor() %>%
  table() %>%
  as_tibble()
colnames(classes)[1] <- "value"

indices <- test_images$class_indices %>%
  as.data.frame() %>%
  gather() %>%
  mutate(value = as.character(value)) %>%
  left_join(classes, by = "value")

test_images$reset()

predictions <-
  predict(model,
          test_images,
          steps = as.integer(test_images$n)
  ) %>%
  round(digits = 2) %>%
  as_tibble()

colnames(predictions) <- indices$key

predictions_test <- predictions %>%
  mutate(truth_idx = as.character(test_images$classes)) %>%
  left_join(indices, by = c("truth_idx" = "value"))

pred_analysis <- predictions_test %>%
  #mutate(img_id = seq(1:test_images$n)) %>%
  mutate(img_id = seq(1:dim(predictions)[1])) %>%
  gather(pred_lbl, y, EdA:F271) %>%
  group_by(img_id) %>%
  filter(y == max(y)) %>%
  arrange(img_id) %>%
  group_by(key, n, pred_lbl) %>%
  count()

pred_analysis <- pred_analysis %>%
  mutate(key = recode(key,
                      EdA = 'Anabaena',
                      F192 = 'Microcystis',
                      F271 = 'Dolichospermum'))

pred_analysis <- pred_analysis %>%
  mutate(pred_lbl = recode(pred_lbl,
                           EdA = 'Anabaena',
                           F192 = 'Microcystis',
                           F271 = 'Dolichospermum'))

p <- pred_analysis %>%
  mutate(percentage_pred = nn / n * 100) %>%
  ggplot(aes(x = key, y = pred_lbl,
             fill = percentage_pred,
             label = paste0(round(percentage_pred, 2),"%"))) +
  geom_tile() +
  scale_fill_continuous() +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_text(color = "white",size=10) +
  labs(x = "True class",
       y = "Predicted class",
       fill = "Percentage\nof \npredictions")+
  theme(
    plot.margin=unit(c(1,1,1,1),"cm"),
    plot.title = element_text(size=35),
    plot.subtitle = element_text(size=20),
    axis.title.x = element_text(size=25,vjust=-2),
    axis.title.y = element_text(size=25,vjust=4),
    axis.text.x = element_text(size=25),
    axis.text.y = element_text(size=25,angle=45,hjust=1,vjust=-1),
    legend.text = element_blank(),
    legend.title = element_blank())

png("Predict_ModelMod_Output_01.png", height = 25, width = 30, units = 'cm', res = 300)
p
dev.off()
