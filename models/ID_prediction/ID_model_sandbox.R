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
library(glue)

#virtualenv_create('C:/Users/Tyler.Harman/AppData/Local/miniconda3/envs/new-reticulate',python=install_python())
#tensorflow::install_tensorflow(envname = "C:/Users/Tyler.Harman/AppData/Local/miniconda3/envs/new-reticulate/",version='2.10-cpu',extra_packages = c('pillow','scipy'))
#reticulate::py_install("numpy<2", envname = "C:/Users/Tyler.Harman/AppData/Local/miniconda3/envs/new-reticulate", method = "virtualenv")
Sys.setenv(RETICULATE_PYTHON="C:\\Users\\Tyler.Harman\\AppData\\Local\\miniconda3\\envs\\new-reticulate\\Scripts\\python.exe")
reticulate::py_config()

#__________________________________________________________________________

#Write from TIFF to PNG - save to main image folder

cell_tif <- ('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F199/20X/Total_Color')
images <- list.files(cell_tif, pattern = "tif", full.name = T)
images_names <- list.files(cell_tif, pattern = "tif", full.name = F)
read_images <- lapply(images, tiff::readTIFF)
sav_dir <- ('D:/CyanoSCOPE_imgs/Draft_Model/id_data/Species/F199/20X/')

for(u in 1:length(images)){
  cell_name <- images_names[[u]]
  rgb.img<-Image(read_images[[u]],colormode = Color)
  new_cell_name<-paste0(sub(".tif", replacement = " ", x=cell_name),".png")
  writeImage(rgb.img,files = paste0(sav_dir, new_cell_name))
}

#______________________________________________________________________________#

####new model####

original_dir<-path("D:/CyanoSCOPE_imgs/Draft_Model/id_data/Genus_Data")
new_base_dir<-path("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model")

make_random_subset <- function(subset_name, num_images_per_category) {
  for (category in c("Dolichospermum", "Microcystis")) {
    # Get all image files for the current category
    all_files <- dir_ls(original_dir,
                        regexp = glue("{category}_AS_20X_color \\([0-9]+\\)\\.png$"))

    # Randomly select the specified number of images
    selected_files <- sample(all_files, num_images_per_category, replace = FALSE)

    # Create the destination directory
    dest_dir <- path(new_base_dir, subset_name, category)
    dir_create(dest_dir)

    # Copy the selected files to the destination directory
    file_copy(selected_files, dest_dir)
  }
}

make_random_subset("train", num_images_per_category = 12800)
make_random_subset("test", num_images_per_category = 3200)

create_validation_set <- function(train_dir, val_dir, val_percentage = 0.2) {
  for (category in c("Dolichospermum", "Microcystis")) {
    # Path to category in training set
    category_train_dir <- path(train_dir, category)

    # Get all image files for the current category in training set
    all_files <- dir_ls(category_train_dir, regexp = "\\.png$")

    # Calculate number of images for validation set
    num_val_images <- round(length(all_files) * val_percentage)

    # Randomly select images for validation set
    val_files <- sample(all_files, num_val_images, replace = FALSE)

    # Create the destination directory for validation set
    category_val_dir <- path(val_dir, category)
    dir_create(category_val_dir)

    # Move selected files to validation directory
    file_move(val_files, category_val_dir)
  }
}

# Set the validation directory path
train_dir <- paste0(new_base_dir,"/train")
val_dir <- paste0(new_base_dir,"/validation")

# Create validation set (20% of training data)
create_validation_set(train_dir, val_dir)

#______________________________________________________________________________#

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models")

model_label<-dir("ID_predict_model/train/")
output_n<-length(model_label)

width<-150
height<-150
target_size<-c(width,height)
rgb<-3


path_train<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/train"
path_valid<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/validation"
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
plot(as.raster(train_images[[1]][[1]][13,,,]))

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

model <- model_function()
model

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(learning_rate = 0.001),
  metrics = "accuracy"
)

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

path_test<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test"

test_data_gen <- image_data_generator(rescale = 1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021)
model %>% evaluate(test_images,
                   steps = test_images$n/batch_size)

test_image <- image_load("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test/F199/F199_AS_20X_color (467).png",
                         target_size = target_size)

x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Species" = model_label, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- format(pred$Probability,scientific = F)
pred$Probability <- paste((100*pred$Probability),"%")
pred

save_model_tf(model, "ID_predict_model/ID_model_02/") #save model here
model <- load_model_tf("ID_predict_model/ID_model_02/")

#_______________________________________________________________________
#true vs. predicted

setwd("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models")

model_label<-dir("ID_predict_model/train/")
output_n<-length(model_label)

width<-150
height<-150
target_size<-c(width,height)
rgb<-3

batch_size<-32
epochs<-6

path_test<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test"

test_data_gen <- image_data_generator(rescale = 0.1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021)

model <- load_model_tf("ID_predict_model/ID_model_02/")

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
  gather(pred_lbl, y, Dolichospermum:Microcystis) %>%
  group_by(img_id) %>%
  filter(y == max(y)) %>%
  arrange(img_id) %>%
  group_by(key, n, pred_lbl) %>%
  count()

pred_analysis_false <- pred_analysis[c(2:3),]

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

png("Predict_Model_Output_03.png", height = 25, width = 30, units = 'cm', res = 300)
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
                          best_results$learning_rate[1],
                        dropoutrate = best_results$dropoutrate[1],
                        n_dense = best_results$n_dense[1])

hist <- model %>% fit(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size,
  epochs = epochs,
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 1
)

save_model_tf(model, "ID_predict_model/ID_model_02mod/")

model <- load_model_tf("ID_predict_model/ID_model_02mod/")

path_test<-"C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/Draft_Model/models/ID_predict_model/test"

test_data_gen <- image_data_generator(rescale = 1/255)
test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = model_label,
                                          shuffle = F,
                                          seed = 2021)
model %>% evaluate(test_images,
                   steps = test_images$n/batch_size)

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
  gather(pred_lbl, y, Dolichospermum:Microcystis) %>%
  group_by(img_id) %>%
  filter(y == max(y)) %>%
  arrange(img_id) %>%
  group_by(key, n, pred_lbl) %>%
  count()

pred_analysis_false <- pred_analysis[c(2:3),]

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

png("Predict_ModelMod_Output_02mod.png", height = 25, width = 30, units = 'cm', res = 300)
p
dev.off()
