Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
py_config()
reticulate::py_install('pillow')

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

cell_tif <- ('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Microcystis_F192/20X/Total_Color')
images <- list.files(cell_tif, pattern = "tif", full.name = T)
images_names <- list.files(cell_tif, pattern = "tif", full.name = F)
read_images <- lapply(images, readTIFF)
sav_dir <- ('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/main_image_folder/')

for(u in 1:length(images)){
  cell_name <- images_names[[u]]
  rgb.img<-Image(read_images[[u]],colormode = Color)
  new_cell_name<-paste0(sub(".tif", replacement = " ", x=cell_name),".png")
  writeImage(rgb.img,files = paste0(sav_dir, new_cell_name))
}

#______________________________________________________________________________#

####new model####

original_dir<-path("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/data/main_image_folder")
new_base_dir<-path("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model")

make_subset_20X<-function(subset_name, start_index, end_index){
  for (category in c("EdA", "F192", "F271")){
    file_name<-glue::glue("{category}_20x_color ({start_index:end_index}).png")
    dir_create(new_base_dir / subset_name / category)
    file_copy(original_dir / file_name ,
              new_base_dir / subset_name / category / file_name)
  }
}

make_subset_40X<-function(subset_name, start_index, end_index){
  for (category in c("EdA", "F192", "F271")){
    file_name<-glue::glue("{category}_40x_color ({start_index:end_index}).png")
    dir_create(new_base_dir / subset_name / category)
    file_copy(original_dir / file_name ,
              new_base_dir / subset_name / category / file_name)
  }
}

make_subset_20X("train", start_index = 1, end_index = 3500)
make_subset_40X("train", start_index = 1, end_index = 750)

make_subset_20X("validation", start_index = 3501, end_index = 4500)
make_subset_40X("validation", start_index = 751, end_index = 1250)

make_subset_20X("test", start_index = 4501, end_index = 8000)
make_subset_40X("test", start_index = 1251, end_index = 2000)

#______________________________________________________________________________#

setwd("X:/CyanoSCOPE_imgs/Draft_Model/models")

model_label<-dir("ID_predict_model/train/")
output_n<-length(model_label)

width<-150
height<-150
target_size<-c(width,height)
rgb<-3


path_train<-"X:/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/train"
path_valid<-"X:/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/validation"
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
                                         color_mode = "rgb")
validation_images <- flow_images_from_directory(path_valid,
                                                valid_data_gen,
                                                target_size = target_size,
                                                class_mode = "categorical",
                                                classes = model_label,
                                                seed = 2021,
                                                color_mode = "rgb")

table(train_images$classes)
plot(as.raster(train_images[[1]][[1]][12,,,]))

mod_base <- application_xception(weights = 'imagenet',
                                 include_top = FALSE, input_shape = c(width, height, 3))
freeze_weights(mod_base)

model_function <- function(learning_rate = 0.001,
                           dropoutrate=0.2, n_dense=150){

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
epochs<-10

hist <- model %>% fit(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size,
  epochs = epochs,
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 1
)

path_test<-"X:/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test"

test_data_gen <- image_data_generator(rescale = 0.1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021)
model %>% evaluate(test_images,
                   steps = test_images$n/batch_size)

test_image <- image_load("X:/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test/EdA/EdA_AS_20X_color (4514).png",
                         target_size = target_size)

x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred

save_model_tf(model, "ID_predict_model/ID_model_test/") #save model here
model <- load_model_tf("ID_predict_model/ID_model_test/")

#_______________________________________________________________________
#true vs. predicted

setwd("X:/CyanoSCOPE_imgs/Draft_Model/models")

model_label<-dir("ID_predict_model/train/")
output_n<-length(model_label)

width<-150
height<-150
target_size<-c(width,height)
rgb<-3

batch_size<-32
epochs<-6

path_test<-"X:/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test"

test_data_gen <- image_data_generator(rescale = 0.1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021)

model <- load_model_tf("ID_predict_model/ID_model_test/")

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

pred_analysis_false <- pred_analysis[2:3,]

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

png("Predict_Model_Output_02.png", height = 25, width = 30, units = 'cm', res = 300)
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
