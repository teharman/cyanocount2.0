Sys.setenv(RETICULATE_PYTHON="C:/Users/Tyler.Harman/AppData/Local/r-miniconda/envs/r-reticulate")
library(reticulate)
py_config()
reticulate::py_install('pillow')

library(keras)
library(tensorflow)
library(tidyverse)
library(fs)
library(tibble)
library(tfdatasets)
#__________________________________________________________________________

data_dir<-path("pets_dataset")
dir_create<-data_dir

data_url<-path("http://www.robots.ox.ac.uk/~vgg/data/pets/data")
for(filename in c ("images.tar.gz", "annotations.tar.gz")) {
#  download.file(url = data_url / filename,
#                destfile = data_dir / filename)
  untar(data_dir/ filename, exdir = data_dir)
}

input_dir<-data_dir / "images"
target_dir<-data_dir / "annotations/trimaps/"

image_paths<-tibble::tibble(
  input = sort(dir_ls(input_dir,glob="*.jpg")),
  target = sort(dir_ls(target_dir,glob="*.png"))
)

tibble::glimpse(image_paths)

display_image_tensor<-function(x,...,max=255,
                               plot_margins = c(0,0,0,0)) {
  if(!is.null(plot_margins))
    par(mar = plot_margins)

  x %>%
    as.array() %>%
    drop() %>%
    as.raster(max=max) %>%
    plot(..., interpolate=FALSE)
}

image_tensor<-image_paths$input[10] %>%
  tf$io$read_file() %>%
  tf$io$decode_jpeg()

str(image_tensor)

display_image_tensor(image_tensor)

display_target_sensor<-function(target)
  display_image_tensor(target - 1, max=2)

target<-image_paths$target[10] %>%
  tf$io$read_file() %>%
  tf$io$decode_png()

str(target)

display_target_sensor(target)

tf_read_image<-function(path, format="image", resize=NULL,...) {
  img<-path %>%
    tf$io$read_file() %>%
    tf$io[[paste0("decode_", format)]](...)
  if (!is.null(resize))
    img <- img %>%
      tf$image$resize(as.integer(resize))

  img
}

img_size<-c(200,200)

tf_read_image_and_resize<-function(..., resize = img_size)
  tf_read_image(..., resize=resize)

make_dataset<-function(paths_df){
  tensor_slices_dataset(paths_df) %>%
    dataset_map(function(path){
      image<-path$input %>%
        tf_read_image_and_resize("jpeg", channels = 3L)
      target<-path$target %>%
        tf_read_image_and_resize("png", channels = 1L)
      target<-target-1
      list(image,target)
    }) %>%
    dataset_cache() %>%
    dataset_shuffle(buffer_size = nrow(paths_df)) %>%
    dataset_batch(32)
}

num_val_samples<-1000
val_idx<-sample.int(nrow(image_paths), num_val_samples)

val_paths<-image_paths[val_idx, ]
train_paths<-image_paths[-val_idx, ]

validation_dataset<-make_dataset(val_paths)
train_dataset<-make_dataset(train_paths)

get_model<-function(img_size, num_classes) {
  conv<-function(..., padding = "same", activation = "relu")
    layer_conv_2d(..., padding = padding, activation = activation)
  conv_transpose<-function(..., padding = "same", activation = "relu")
    layer_conv_2d_transpose(..., padding = padding, activation = activation)
  input<-layer_input(shape=c(img_size, 3))
  output<-input %>%
    layer_rescaling(scale = 1/255) %>%
    conv(64, 3, strides= 2) %>%
    conv(64, 3) %>%
    conv(128, 3, strides= 2) %>%
    conv(128, 3) %>%
    conv(256, 3, strides= 2) %>%
    conv(256, 3) %>%
    conv_transpose(256, 3) %>%
    conv_transpose(256, 3, strides = 2) %>%
    conv_transpose(128, 3) %>%
    conv_transpose(128, 3, strides = 2) %>%
    conv_transpose(64, 3) %>%
    conv_transpose(64, 3, strides = 2) %>%
    conv(num_classes, 3, activation = "softmax")
  keras_model(input,output)
}

target_model<-get_model(img_size = img_size, num_classes = 3)

target_model

