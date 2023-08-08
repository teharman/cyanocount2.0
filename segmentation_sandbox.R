Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
py_config()
reticulate::py_install('pillow')

library(reticulate)
library(keras)
library(tensorflow)
library(tidyverse)
library(fs)
library(tibble)
library(rsample)
library(tfdatasets)
library(unet)
library(EBImage)
library(platypus)
#______________________________________________________________________________#
setwd('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/')
data_dir<-path("segmentation_model/")

tiff_dir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/segmentation_model/TIFF_imgs")
JPEG_savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/segmentation_model/input_imgs/")
images <- list.files(tiff_dir, pattern = "tif", full.name = T)
images_names <- list.files(tiff_dir, pattern = "tif", full.name = F)
imgNames <- paste0(images_names)

for (y in 1:length(images)){
  img <- readTIFF(images[[y]])
  img_transposed <- aperm(img,c(2,1,3))
  rgb.img<-Image(img_transposed,colormode = Color)
  analyzed_image<-paste0(sub(").tif", replacement = ")", x=imgNames[[y]]),".png")
  writeImage(rgb.img,files = paste0(JPEG_savdir, analyzed_image))
}
#______________________________________________________________________________#
gc()

setwd('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/')
data_dir<-path("segmentation_model/")

input_dir<-data_dir / "input_imgs"
target_dir<-data_dir / "true_mask"

model <- unet(input_shape = c(256, 256, 3))

images <- tibble(
  img = list.files(input_dir, full.names = TRUE),
  mask = list.files(target_dir, full.names = TRUE)
)

data <- tibble(
  img = list.files(input_dir, full.names = TRUE),
  mask = list.files(target_dir, full.names = TRUE)
)

data <- initial_split(data, prop = 0.8)

training_dataset <- training(data) %>%
  tensor_slices_dataset() %>%
  dataset_map(~.x %>% list_modify(
    # decode_jpeg yields a 3d tensor of shape (1280, 1918, 3)
    img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
    # decode_gif yields a 4d tensor of shape (1, 1280, 1918, 3),
    # so we remove the unneeded batch dimension and all but one
    # of the 3 (identical) channels
    mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
  ))

example <- training_dataset %>% as_iterator() %>% iter_next()
example

training_dataset <- training_dataset %>%
  dataset_map(~.x %>% list_modify(
    img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
    mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
  ))

training_dataset <- training_dataset %>%
  dataset_map(~.x %>% list_modify(
    img = tf$image$resize(.x$img, size = shape(256,256)),
    mask = tf$image$resize(.x$mask, size = shape(256,256))
  ))

example <- training_dataset %>% as_iterator() %>% iter_next()
example

random_bsh <- function(img) {
  img %>%
    tf$image$random_brightness(max_delta = 0.3) %>%
    tf$image$random_contrast(lower = 0.5, upper = 0.7) %>%
    tf$image$random_saturation(lower = 0.5, upper = 0.7) %>%
    # make sure we still are between 0 and 1
    tf$clip_by_value(0, 1)
}

training_dataset <- training_dataset %>%
  dataset_map(~.x %>% list_modify(
    img = random_bsh(.x$img)
  ))

example <- training_dataset %>% as_iterator() %>% iter_next()
example$img %>% as.array() %>% display()
example$mask %>% as.array() %>% display()


create_dataset <- function(data, train, batch_size = 8L) {

  dataset <- data %>%
    tensor_slices_dataset() %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
      mask = tf$image$decode_gif(tf$io$read_file(.x$mask))[1,,,][,,1,drop=FALSE]
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
      mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32)
    )) %>%
    dataset_map(~.x %>% list_modify(
      img = tf$image$resize(.x$img, size = shape(384, 384)),
      mask = tf$image$resize(.x$mask, size = shape(384, 384))
    ))

  # data augmentation performed on training set only
  if (train) {
    dataset <- dataset %>%
      dataset_map(~.x %>% list_modify(
        img = random_bsh(.x$img)
      ))
  }

  # shuffling on training set only
  if (train) {
    dataset <- dataset %>%
      dataset_shuffle(buffer_size = batch_size)
  }

  # train in batches; batch size might need to be adapted depending on
  # available memory
  dataset <- dataset %>%
    dataset_batch(batch_size)

  dataset %>%
    # output needs to be unnamed
    dataset_map(unname)
}

training_dataset <- create_dataset(training(data), train = TRUE)
validation_dataset <- create_dataset(testing(data), train = FALSE)

model <- unet(input_shape = c(256, 256, 3),
              num_classes = 2)
summary(model)

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy"
)

datagen<-segmentation_generator(
  path = "segmentation_model",
  colormap = binary_colormap,
  only_images = F,
  mode = "dir",
  net_h = 256,
  net_w = 256,
  grayscale = F,
  batch_size = 8,
  shuffle = F,
  subdirs = c("/input_imgs","/true_mask")
)

history<-model%>%
  fit_generator(
    datagen,
    epochs=5,
    steps_per_epoch = 24,
    verbose = 1
  )

save_model_tf(model, "segmentation_model/initial_test_model/") #save model here

model<-load_model_tf('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/segmentation_model/initial_test_model/', custom_objects = NULL, compile = TRUE)

test_img<-image_load("segmentation_model/input_imgs/20x_F192 (19).png",target_size = c(256,256))
test_img%>%image_to_array()%>%
  '/'(255)%>%
  as.raster()%>%
  plot()
test_array<-test_img%>%image_to_array()%>%
  array_reshape(.,c(1,dim(.)))%>%
  '/'(255)

mask<-model%>%predict(test_array)%>%
  get_masks(binary_colormap)
plot(as.raster(mask[[1]]/255))
