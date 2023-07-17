Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
library(reticulate)
py_config()
reticulate::py_install('pillow')

library(keras)
library(tensorflow)
library(tidyverse)
library(fs)
library(tibble)
library(rsample)
library(tfdatasets)
library(unet)
#______________________________________________________________________________#
setwd('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/')
data_dir<-path("segmentation_model/")

input_dir<-data_dir / "input_imgs"
target_dir<-data_dir / "true_mask"

model <- unet(input_shape = c(384, 384, 3))

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
    img = tf$image$resize(.x$img, size = shape(384, 384)),
    mask = tf$image$resize(.x$mask, size = shape(384, 384))
  ))

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
example$img %>% as.array() %>% as.raster() %>% plot()

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

model <- unet(input_shape = c(384, 384, 3),
              num_classes = 1,
              output_activation = "softmax")
summary(model)

dice <- custom_metric("dice", function(y_true, y_pred, smooth = 1.0) {
  y_true_f <- k_flatten(y_true)
  y_pred_f <- k_flatten(y_pred)
  intersection <- k_sum(y_true_f * y_pred_f)
  (2 * intersection + smooth) / (k_sum(y_true_f) + k_sum(y_pred_f) + smooth)
})

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = list(dice, metric_binary_accuracy)
)

model |>
  fit(
    training_dataset, epochs = 30,
    validation_data = validation_dataset
  )
