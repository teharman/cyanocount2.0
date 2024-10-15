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
library(tiff)
library(png)
library(pixmap)
library(raster)
library(cellcount)
library(dplyr)
library(jjb)
library(shiny)
library(magick)
library(ggplot2)
library(shinyalert)
library(shinyFiles)
library(shinyjs)
library(shinyBS)
library(DT)
library(shinythemes)
library(cyanocount2.0)
library(RSAGA)
library(spsComps)
library(stringr)
library(pbapply)

img_dir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/Pipeline_Test/AccuScope/Batch_01/")

images_list <- data.frame(image_names = character(0))
images <- list.files(img_dir, full.name = T)
image_names <- list.files(img_dir, full.name = F)
images_list <- data.frame(image_names)

read_images <- lapply(images, EBImage::readImage)

ID.input <- data.frame(file_ID = character(0), cell_number = numeric(0), ID_estimate = character(0), Percent_estimate = numeric(0))
shape.input <- data.frame(file_ID = character(0), cell_number = numeric(0), shape_estimate = character(0), Percent_estimate = numeric(0))

cell.coord <- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0))
cell.count <- data.frame(image_name = character(0), Microcystis_count = numeric(0), Anabaena_count = numeric(0), Dolichospermum_count = numeric(0))

segmentation_model <- load_model_tf('./models/binary_segmentation/updated_test_model_7/', custom_objects = NULL, compile = TRUE)
test_img <- image_load(images[[1]],target_size = c(1024,1024))
new_array <- test_img %>% image_to_array() %>% array_reshape(.,c(1,dim(.))) %>% '/'(255)
mask_main <- segmentation_model %>% predict(new_array) %>%
  get_masks(binary_colormap)
for (z in 2:length(read_images)){
  test_img <- image_load(images[[z]],target_size = c(1024,1024))
  new_array <- test_img %>% image_to_array() %>% array_reshape(.,c(1,dim(.))) %>% '/'(255)
  mask <- segmentation_model %>% predict(new_array) %>%
    get_masks(binary_colormap)
  mask_main <- append(mask_main, mask)
}

EBImage::display(mask_main[[7]])

shape_model <- load_model_tf('./models/shape_model/shape_model01/', custom_objects = NULL, compile = TRUE)
predict_model <- load_model_tf('./models/ID_prediction/ID_model_02mod/', custom_objects = NULL, compile = TRUE)
watershed_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 100, tolerance= 0.4, ext = 1, removeEdgeCells = TRUE) {
  if (removeEdgeCells == TRUE){
    image <- thresh(x, w = w, h = h, offset = offset)
    image1 <- fillHull(image)
    image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
    nf <- computeFeatures.shape(image2)
    nr <- which(nf[, "s.area"] < areathresh)
    image3 <- rmObjects(image2, nr)
    dims <- dim(image3)
    border1 <- c(image3[1:dims[1], 1], image3[1:dims[1], dims[2]], image3[1, 1:dims[2]], image3[dims[1], 1:dims[2]])
    ids <- unique(border1[which(border1 != 0)])
    inner <- rmObjects(image3, ids)
    return(inner)
  } else{
    image <- thresh(x, w = w, h = h, offset = offset)
    image1 <- fillHull(image)
    image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
    nf <- computeFeatures.shape(image2)
    nr <- which(nf[, "s.area"] < areathresh)
    image3 <- rmObjects(image2, nr)
    return(image3)
  }
}
single_cell_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
  image <- thresh(x, w = w, h = h, offset = offset)
  image1 <- fillHull(image)
  image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
  nf <- computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < areathresh)
  image3 <- rmObjects(image2, nr)
  return(image3)
}

test_img <- image_load(images[[1]],target_size = c(1024,1024))
img_array <- test_img %>% image_to_array() %>% '/'(255)
rgb.imgs <- Image(img_array,colormode = Color)
mask <- abind(mask_main[[1]])
mask <- mask[,,1]
EBImage::display(mask)
img_watershed <- watershed_convert(mask,w=17,h=17,offset=0.001,areathresh=50,tolerance=1,ext = 1,removeEdgeCells=TRUE)
EBImage::display(img_watershed)
final_img <- count_images(img_watershed,normalize = T, removeEdgeCells = T)
EBImage::display(final_img)
count_cells(img_watershed)
ctmask <- opening(img_watershed>0.1,makeBrush(5,shape='disc'))
seed_mask <- single_cell_convert(ctmask,w=17,h=17,offset=0.001,areathresh=50,tolerance=1,ext = 1)
final_img <- count_images(seed_mask,normalize = T, removeEdgeCells = T)
EBImage::display(final_img)
count_cells(img_watershed)
cmask <- propagate(mask,seeds=seed_mask,mask=ctmask,lambda = 10^1)
cmask1 <- array_reshape(cmask,c(dim(cmask),1))
segmented <- paintObjects(cmask,rgb.imgs,col = c('black','orange'))
EBImage::display(segmented)
st_blob <- stackObjects(cmask,img_watershed)
st_img <- stackObjects(cmask,rgb.imgs)
st_img_test <- Image(st_img)
st_img_blob <- Image(st_blob)
EBImage::display(st_img_test)
EBImage::display(st_img_blob)
cell_seg <- list(st_img_test)
blob_seg <- list(st_img_blob)
coord.mtx <- RSAGA::grid.to.xyz(cmask)
filter <- filter(coord.mtx,z>0)
for(u in 1:dim(st_img_test)[4]){
  filter.mtx <- subset(filter,z==u)
  xmin <- min(filter.mtx$x)-1
  xmax <- max(filter.mtx$x)+1
  ymin <- min(filter.mtx$y)-1
  ymax <- max(filter.mtx$y)+1
  cell.coord[nrow(cell.coord) + 1, ] <- c(xmin, xmax, ymin, ymax)
  rm(filter.mtx)
}
rm(coord.mtx)
rm(filter)

shape.features.data <- read.csv('Cell_Features_Data.csv',header=T)

features.data.img <- computeFeatures.shape(cmask,rgb.imgs)
features.data.img <- as.data.frame(features.data.img)
features.data.img$file_ID <- image_names[[1]]
features.data.img <- features.data.img %>% dplyr::select(.,file_ID,
                                                         s.area,
                                                         s.perimeter,
                                                         s.radius.mean)
features.data.img$cell_number <- 1:nrow(features.data.img)

for (z in 2:length(read_images)){
  test_img <- image_load(images[[z]],target_size = c(1024,1024))
  img_array <- test_img %>% image_to_array() %>% '/'(255)
  rgb.imgs <- Image(img_array,colormode = Color)
  mask <- abind(mask_main[[z]])
  mask <- mask[,,1]
  EBImage::display(mask)
  img_watershed <- watershed_convert(mask,w=17,h=17,offset=0.001,areathresh=50,tolerance=1,ext = 1,removeEdgeCells=TRUE)
  EBImage::display(img_watershed)
  ctmask <- opening(img_watershed>0.1,makeBrush(5,shape='disc'))
  seed_mask <- single_cell_convert(ctmask,w=17,h=17,offset=0.001,areathresh=50,tolerance=1,ext = 1)
  cmask <- propagate(mask,seeds=seed_mask,mask=ctmask,lambda = 10^1)
  EBImage::display(cmask)
  cmask1 <- array_reshape(cmask,c(dim(cmask),1))
  segmented <- paintObjects(cmask,rgb.imgs,col = c('black','orange'))
  EBImage::display(segmented)
  st_img <- stackObjects(cmask,rgb.imgs)
  st_blob <- stackObjects(cmask,img_watershed)
  st_img_test <- Image(st_img)
  st_img_blob <- Image(st_blob)
  EBImage::display(st_img_blob)
  seg_cell <- list(st_img)
  seg_blob <- list(st_blob)
  seg_cell_test <- Image(seg_cell)
  seg_blob_test <- Image(seg_blob)
  cell_seg <- append(cell_seg,seg_cell_test)
  blob_seg <- append(blob_seg,seg_blob_test)
  coord.mtx <- RSAGA::grid.to.xyz(cmask)
  filter <- filter(coord.mtx,z>0)
  for(u in 1:dim(st_img_test)[4]){
    filter.mtx <- subset(filter,z==u)
    xmin <- min(filter.mtx$x)-1
    xmax <- max(filter.mtx$x)+1
    ymin <- min(filter.mtx$y)-1
    ymax <- max(filter.mtx$y)+1
    cell.coord[nrow(cell.coord) + 1, ] <- c(xmin, xmax, ymin, ymax)
    rm(filter.mtx)
  }
  rm(coord.mtx)
  rm(filter)
  features.data.img1 <- computeFeatures.shape(cmask,rgb.imgs)
  features.data.img1 <- as.data.frame(features.data.img1)
  features.data.img1$file_ID <- image_names[[z]]
  features.data.img1 <- features.data.img1 %>% dplyr::select(.,file_ID,
                                                           s.area,
                                                           s.perimeter,
                                                           s.radius.mean)
  features.data.img1$cell_number <- 1:nrow(features.data.img1)
  features.data.img <- rbind(features.data.img,features.data.img1)
  rm(features.data.img1)
}

#add cell numbers to features.data.img data.frames for each image - helpful to merge into final data.frame

for(k in 1:length(blob_seg)) {
  blob_img <- blob_seg[[k]]
  file_ID <- image_names[[k]]
  for(z in 1:dim(blob_img)[3]){
    cell_num <- z
    resize_img <- EBImage::resize(blob_img[,,z],w=150,h=150)
    x <- image_to_array(resize_img)
    x1 <- Image(x,colormode = Grayscale)
    x2 <- array_reshape(x1, c(1, dim(x1)))
    pred <- shape_model %>% predict(x2)
    model_label<-c("Dolichospermum","Microcystis")
    pred <- data.frame("Species" = model_label, "Probability" = t(pred))
    pred <- pred[order(pred$Probability, decreasing=T),][1:3,]
    pred$Probability <- (100*pred$Probability)
    pred$Probability <- paste(formatC(pred$Probability, digits = 3, format = "f"),"%")
    shape.input[nrow(shape.input) + 1, ] <- c(file_ID, cell_num, pred$Species[[1]], pred$Probability[[1]])
  }
}

for(k in 1:length(cell_seg)) {
  cell_img <- cell_seg[[k]]
  file_ID <- image_names[[k]]
  for(z in 1:dim(cell_img)[4]){
    cell_num <- z
    resize_img <- EBImage::resize(cell_img[,,,z],w=150,h=150)
    x <- image_to_array(resize_img)
    x1 <- Image(x,colormode = Color)
    x2 <- array_reshape(x1, c(1, dim(x1)))
    pred <- predict_model %>% predict(x2)
    model_label<-c("Dolichospermum","Microcystis")
    pred <- data.frame("Species" = model_label, "Probability" = t(pred))
    pred <- pred[order(pred$Probability, decreasing=T),][1:3,]
    pred$Probability <- (100*pred$Probability)
    pred$Probability <- paste(formatC(pred$Probability, digits = 3, format = "f"),"%")
    ID.input[nrow(ID.input) + 1, ] <- c(file_ID, cell_num, pred$Species[[1]], pred$Probability[[1]])
  }
}

#secondary_analysis <- data.frame(file_ID = character(0), cell_number = numeric(0), shape_estimate = character(0), shape_percent = character(0), ID_estimate = character(0), ID_percent = character(0))
#for(i in 1:dim(shape.input)[[1]]){
#  shape.subset <- shape.input[i,c(3,4)]
#  ID.subset <- ID.input[i,c(3,4)]
#  if(shape.subset$shape_estimate[1] != ID.subset$ID_estimate[1]){
#    file_ID <- ID.input$file_ID[[i]]
#    cell_number <- ID.input$cell_number[[i]]
#    shape_estimate <- shape.subset[[1,1]]
#    ID_estimate <- ID.subset[[1,1]]
#    shape_percent <- shape.subset[[1,2]]
#    ID_percent <- ID.subset[[1,2]]
#    secondary_analysis[nrow(secondary_analysis) + 1, ] <- c(file_ID, cell_number, shape_estimate, shape_percent, ID_estimate, ID_percent)
#  }
#}

total_results <- merge(shape.input,ID.input,by=c("file_ID","cell_number"))
colnames(total_results)[4] <- 'Shape.Estimate'
colnames(total_results)[6] <- 'ID.Estimate'

prediction_labels <- data.frame(prediction_results = character(0))
for(i in 1:dim(total_results)[1]){
  value1 <- as.numeric(paste0(sub(" %", replacement = "", x=total_results[i,4])))
  value2 <- as.numeric(paste0(sub(" %", replacement = "", x=total_results[i,6])))
  predict1 <- total_results[i,3]
  predict2 <- total_results[i,5]

  value_result1 <- (value1 < 75)
  value_result2 <- (value2 < 75)
  value_result <- as.logical(value_result1 + value_result2)

  try(if(predict1 == predict2 & value_result == FALSE){
    value1 <- "POSITIVE"
    prediction_labels[nrow(prediction_labels) + 1, ] <- c(value1)
  })
  try(if(predict1 != predict2 & value_result == FALSE){
    value3 <- "UNDETERMINED"
    prediction_labels[nrow(prediction_labels) + 1, ] <- c(value3)
  })
  try(if(predict1 == predict2 & value_result == TRUE){
    value3 <- "ESTIMATE"
    prediction_labels[nrow(prediction_labels) + 1, ] <- c(value3)
  })
  try(if(predict1 != predict2 & value_result == TRUE){
    value4 <- "NEGATIVE"
    prediction_labels[nrow(prediction_labels) + 1, ] <- c(value4)
  })
}

total_results <- cbind(total_results,prediction_labels)

total_results <- merge(total_results,features.data.img,by=c("file_ID","cell_number"))
total_results$cell_number <- as.character(total_results$cell_number)
total_results <- total_results %>%
  mutate(cell_number = recode(cell_number,
                        '1' = '01',
                        '2' = '02',
                        '3' = '03',
                        '4' = '04',
                        '5' = '05',
                        '6' = '06',
                        '7' = '07',
                        '8' = '08',
                        '9' = '09'
  ))
total_results <- total_results[order(total_results$cell_number,decreasing=FALSE),]
total_results <- total_results[order(total_results$file_ID,decreasing=FALSE),]

total_results <- cbind(total_results,cell.coord)

analysis_type <- "20X"
new_shape.features.data <- subset(shape.features.data,Objective==analysis_type)

for(y in 1:dim(total_results)[1]){
  cell_row_data <- total_results[y,]
  cell_row_data$s.area <- as.numeric(cell_row_data$s.area)
  if(cell_row_data$ID_estimate == "Microcystis"){
    if (between(cell_row_data$s.area, new_shape.features.data$SD_Minus2[4], new_shape.features.data$SD_Plus2[4])==FALSE) {
      total_results$ID_estimate[y] <- "Unknown"
      total_results$prediction_results[y] <- "POSITIVE"
    } else{}
  } else if(cell_row_data$ID_estimate == "Dolichospermum"){
    if (between(cell_row_data$s.area, new_shape.features.data$SD_Minus2[1], new_shape.features.data$SD_Plus2[1])==FALSE) {
      total_results$ID_estimate[y] <- "Unknown"
      total_results$prediction_results[y] <- "POSITIVE"
    } else{}
  }
}

for(y in 1:dim(total_results)[1]){
  cell_row_data <- total_results[y,]
  if(cell_row_data$prediction_results == "UNDETERMINED"){
    total_results$ID_estimate[y] <- "Undetermined"
    total_results$prediction_results[y] <- "ESTIMATE"
  } else{}
}

for(y in 1:dim(total_results)[1]){
  cell_row_data <- total_results[y,]
  if(cell_row_data$prediction_results == "NEGATIVE"){
    total_results$ID_estimate[y] <- "Unknown"
    total_results$prediction_results[y] <- "POSITIVE"
  } else{}
}

test_ID.input <- subset(total_results,file_ID==image_names[[1]])
test_img <- image_load(images[[1]],target_size = c(1024,1024))
img_array <- test_img %>% image_to_array() %>% '/'(255)
rgb.imgs <- Image(img_array,colormode = Color)
test_img1 <- magick::image_ggplot(image_read(EBImage::flop(EBImage::rotate(rgb.imgs,90))))

draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 14)

  grid::rectGrob(
    width = grid::unit(0.8, "npc"),
    height = grid::unit(0.8, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}

GeomRect$draw_key = draw_key_polygon3

if(length(unique(test_ID.input$prediction_results))==2){
  estimate_plot <- test_img1+geom_rect(data = test_ID.input,
                                       aes(xmin = xmin, xmax = xmax,
                                           ymin = ymin, ymax = ymax,
                                           fill = ID_estimate, colour = ID_estimate,linetype = prediction_results),
                                       alpha = .20, linewidth = 0.25, inherit.aes = FALSE)+
    scale_colour_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
    scale_fill_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
    scale_linetype_manual(name="Prediction Results",
                          labels = c("Estimate", "Positive"),
                          values = c(2,1))+
    guides(linetype = guide_legend(override.aes = list(
      linetype = c("dashed", "solid"),
      color = c("black","black"),
      fill = c(NA,NA)))) +
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=20),
          legend.key.size = unit(0.75,"cm"),
          legend.spacing.y = unit(5, 'mm'),
          #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
          #                          colour = c("black","black","black",NA)),
          legend.position = "right")
  estimate_list <- list(estimate_plot)
} else{
  estimate_plot <- test_img1+geom_rect(data = test_ID.input,
                                       aes(xmin = xmin, xmax = xmax,
                                           ymin = ymin, ymax = ymax,
                                           fill = ID_estimate, colour = ID_estimate,linetype = prediction_results),
                                       alpha = .20, linewidth = 0.25, inherit.aes = FALSE)+
    scale_colour_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
    scale_fill_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
    scale_linetype_manual(name="Prediction Results",
                          labels = c("Positive"),
                          values = c(1))+
    guides(linetype = guide_legend(override.aes = list(
      linetype = c("solid"),
      color = c("black"),
      fill = c(NA)))) +
    theme(legend.text = element_text(size=15),
          legend.title = element_text(size=20),
          legend.key.size = unit(0.75,"cm"),
          legend.spacing.y = unit(5, 'mm'),
          #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
          #                          colour = c("black","black","black",NA)),
          legend.position = "right")
  estimate_list <- list(estimate_plot)
}

for(r in 2:length(read_images)) {
  test_ID.input <- subset(total_results,file_ID==image_names[[r]])
  test_img <- image_load(images[[r]],target_size = c(1024,1024))
  img_array <- test_img %>% image_to_array() %>% '/'(255)
  rgb.imgs <- Image(img_array,colormode = Color)
  test_img1 <- magick::image_ggplot(image_read(EBImage::flop(EBImage::rotate(rgb.imgs,90))))

  draw_key_polygon3 <- function(data, params, size) {
    lwd <- min(data$size, min(size) / 14)

    grid::rectGrob(
      width = grid::unit(0.8, "npc"),
      height = grid::unit(0.8, "npc"),
      gp = grid::gpar(
        col = data$colour,
        fill = alpha(data$fill, data$alpha),
        lty = data$linetype,
        lwd = lwd * .pt,
        linejoin = "mitre"
      ))
  }

  GeomRect$draw_key = draw_key_polygon3

  if(length(unique(test_ID.input$prediction_results))==2){
    estimate_plot <- test_img1+geom_rect(data = test_ID.input,
                                         aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax,
                                             fill = ID_estimate, colour = ID_estimate,linetype = prediction_results),
                                         alpha = .20, linewidth = 0.25, inherit.aes = FALSE)+
      scale_colour_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
      scale_fill_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
      scale_linetype_manual(name="Prediction Results",
                            labels = c("Estimate", "Positive"),
                            values = c(2,1))+
      guides(linetype = guide_legend(override.aes = list(
        linetype = c("dashed", "solid"),
        color = c("black","black"),
        fill = c(NA,NA)))) +
      theme(legend.text = element_text(size=15),
            legend.title = element_text(size=20),
            legend.key.size = unit(0.75,"cm"),
            legend.spacing.y = unit(5, 'mm'),
            #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
            #                          colour = c("black","black","black",NA)),
            legend.position = "right")
    estimate_list1 <- list(estimate_plot)
    estimate_list <- append(estimate_list,estimate_list1)
  } else{
    estimate_plot <- test_img1+geom_rect(data = test_ID.input,
                                         aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax,
                                             fill = ID_estimate, colour = ID_estimate,linetype = prediction_results),
                                         alpha = .20, linewidth = 0.25, inherit.aes = FALSE)+
      scale_colour_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
      scale_fill_manual(values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'Undetermined' = "darkred",'Unknown' = "black"))+
      scale_linetype_manual(name="Prediction Results",
                            labels = c("Positive"),
                            values = c(1))+
      guides(linetype = guide_legend(override.aes = list(
        linetype = c("solid"),
        color = c("black"),
        fill = c(NA)))) +
      theme(legend.text = element_text(size=15),
            legend.title = element_text(size=20),
            legend.key.size = unit(0.75,"cm"),
            legend.spacing.y = unit(5, 'mm'),
            #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
            #                          colour = c("black","black","black",NA)),
            legend.position = "right")
    estimate_list1 <- list(estimate_plot)
    estimate_list <- append(estimate_list,estimate_list1)
  }
}

plot(estimate_list[[1]])

estimate_results <- subset(total_results,prediction_labels == c("TRUE","ESTIMATE"))

#estimate_cell_num <- as.numeric(dim(estimate_results)[1])
#cell.den <- cellcount::cell_density(estimate_cell_num, FOV = 0.0726, images = 10, filtration.area = 213.8, volume = 1, total.volume = 5)
