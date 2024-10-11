library(dplyr)
library(tidyr)
library(tidyverse)

####Dolichospermum 20X Data####

Doli_F271 <- ('D:/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F271/20X/Raw_Imgs/')

Doli_F271_BatchFolders <- list.files(Doli_F271)

main_data1 <- list()
for(i in 1:length(Doli_F271_BatchFolders)){
  #if(i==8) next
  csv_batch <- list()
  Batch_Folder <- Doli_F271_BatchFolders[[i]]
  Batch_Folder_path <- paste0(Doli_F271,Batch_Folder,"/")
  Image_files <- list.files(path=Batch_Folder_path)
  if(grepl("(?i).jpg", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".jpg"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tif", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tif"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tiff", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tiff"), invert=TRUE, value=TRUE))
  }
  for(h in 1:length(Image_files)){
    Image_file_folder <- Image_files[[h]]
    Image_folder_path <- paste0(Batch_Folder_path,Image_file_folder,"/")
    Image_csv_data <- list.files(Image_folder_path,pattern=".csv")
    main_csv_path <- paste0(Image_folder_path,Image_csv_data[[1]])
    MAIN_CSV_DATA <- read.csv(main_csv_path,header=T)
    MAIN_CSV_DATA <- MAIN_CSV_DATA %>% dplyr::select(.,x.0.s.area,
                                                     x.0.s.perimeter,
                                                     x.0.s.radius.mean)
    csv_image_list <- list(MAIN_CSV_DATA)
    csv_batch <- append(csv_batch,csv_image_list)
    rm(csv_image_list)
  }
  csv_batch <- do.call('rbind',csv_batch)
  csv_batch_list <- list(csv_batch)
  main_data1 <- append(main_data1,csv_batch_list)
  rm(csv_batch_list)
}

Doli_F199 <- ('D:/CyanoSCOPE_imgs/AccuScope/Dolichospermum_F199/20X/Raw_Imgs/')

Doli_F199_BatchFolders <- list.files(Doli_F199)

main_data2 <- list()
for(i in 1:length(Doli_F199_BatchFolders)){
  csv_batch <- list()
  Batch_Folder <- Doli_F199_BatchFolders[[i]]
  Batch_Folder_path <- paste0(Doli_F199,Batch_Folder,"/")
  Image_files <- list.files(path=Batch_Folder_path)
  if(grepl("(?i).jpg", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".jpg"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tif", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tif"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tiff", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tiff"), invert=TRUE, value=TRUE))
  }
  for(h in 1:length(Image_files)){
    Image_file_folder <- Image_files[[h]]
    Image_folder_path <- paste0(Batch_Folder_path,Image_file_folder,"/")
    Image_csv_data <- list.files(Image_folder_path,pattern=".csv")
    main_csv_path <- paste0(Image_folder_path,Image_csv_data[[1]])
    MAIN_CSV_DATA <- read.csv(main_csv_path,header=T)
    MAIN_CSV_DATA <- MAIN_CSV_DATA %>% dplyr::select(.,x.0.s.area,
                                                     x.0.s.perimeter,
                                                     x.0.s.radius.mean)
    csv_image_list <- list(MAIN_CSV_DATA)
    csv_batch <- append(csv_batch,csv_image_list)
    rm(csv_image_list)
  }
  csv_batch <- do.call('rbind',csv_batch)
  csv_batch_list <- list(csv_batch)
  main_data2 <- append(main_data2,csv_batch_list)
  rm(csv_batch_list)
}

main_data1 <- do.call('rbind',main_data1)
main_data2 <- do.call('rbind',main_data2)

main_data <- rbind(main_data1,main_data2)

#Area
cell_area_avg <- mean(main_data$x.0.s.area,na.rm=TRUE)
cell_area_std <- as.numeric(sd(main_data$x.0.s.area,na.rm=TRUE))
cell_area_1QR <- (cell_area_avg)-(2*cell_area_std)
cell_area_3QR <- (cell_area_avg)+(2*cell_area_std)

#Perimeter
cell_peri_avg <- mean(main_data$x.0.s.perimeter,na.rm=TRUE)
cell_peri_std <- as.numeric(sd(main_data$x.0.s.perimeter,na.rm=TRUE))
cell_peri_1QR <- (cell_peri_avg)-(2*cell_peri_std)
cell_peri_3QR <- (cell_peri_avg)+(2*cell_peri_std)

#Radius
cell_radius_avg <- mean(main_data$x.0.s.radius.mean,na.rm=TRUE)
cell_radius_std <- as.numeric(sd(main_data$x.0.s.radius.mean,na.rm=TRUE))
cell_radius_1QR <- (cell_radius_avg)-(2*cell_radius_std)
cell_radius_3QR <- (cell_radius_avg)+(2*cell_radius_std)

#Compile 20X Data
Main_Shape_Data <- data.frame(Genera = character(0),Objective = character(0),Data.Type=character(0),Average=numeric(0),Standard.Dev=numeric(0),SD_Minus2=numeric(0),SD_Plus2=numeric(0))
Main_Shape_Data[nrow(Main_Shape_Data) + 1, ] <- c('Dolichospermum','20X','Area',cell_area_avg,cell_area_std,cell_area_1QR,cell_area_3QR)
Main_Shape_Data[nrow(Main_Shape_Data) + 1, ] <- c('Dolichospermum','20X','Perimeter',cell_peri_avg,cell_peri_std,cell_peri_1QR,cell_peri_3QR)
Main_Shape_Data[nrow(Main_Shape_Data) + 1, ] <- c('Dolichospermum','20X','Radius',cell_radius_avg,cell_radius_std,cell_radius_1QR,cell_radius_3QR)

rm(list = setdiff(ls(),c("Main_Shape_Data")))

####Microcystis 20X Data####

Micro_F192 <- ('D:/CyanoSCOPE_imgs/AccuScope/Microcystis_F192/20X/Raw_Imgs/')

Micro_F192_BatchFolders <- list.files(Micro_F192)

main_data1 <- list()
for(i in 1:length(Micro_F192_BatchFolders)){
  csv_batch <- list()
  Batch_Folder <- Micro_F192_BatchFolders[[i]]
  Batch_Folder_path <- paste0(Micro_F192,Batch_Folder,"/")
  Image_files <- list.files(path=Batch_Folder_path)
  if(grepl("(?i).jpg", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".jpg"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tif", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tif"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tiff", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tiff"), invert=TRUE, value=TRUE))
  }
  for(h in 1:length(Image_files)){
    Image_file_folder <- Image_files[[h]]
    Image_folder_path <- paste0(Batch_Folder_path,Image_file_folder,"/")
    Image_csv_data <- list.files(Image_folder_path,pattern=".csv")
    main_csv_path <- paste0(Image_folder_path,Image_csv_data[[1]])
    MAIN_CSV_DATA <- read.csv(main_csv_path,header=T)
    MAIN_CSV_DATA <- MAIN_CSV_DATA %>% dplyr::select(.,x.0.s.area,
                                                     x.0.s.perimeter,
                                                     x.0.s.radius.mean)
    csv_image_list <- list(MAIN_CSV_DATA)
    csv_batch <- append(csv_batch,csv_image_list)
    rm(csv_image_list)
  }
  csv_batch <- do.call('rbind',csv_batch)
  csv_batch_list <- list(csv_batch)
  main_data1 <- append(main_data1,csv_batch_list)
  rm(csv_batch_list)
}

Micro_F108 <- ('D:/CyanoSCOPE_imgs/AccuScope/Microcystis_F108/20X/Raw_Imgs/')

Micro_F108_BatchFolders <- list.files(Micro_F108)

main_data2 <- list()
for(i in 1:length(Micro_F108_BatchFolders)){
  if(i==6) next
  if(i==7) next
  if(i==8) next
  if(i==9) next
  if(i==10) next
  csv_batch <- list()
  Batch_Folder <- Micro_F108_BatchFolders[[i]]
  Batch_Folder_path <- paste0(Micro_F108,Batch_Folder,"/")
  Image_files <- list.files(path=Batch_Folder_path)
  if(grepl("(?i).jpg", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".jpg"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tif", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tif"), invert=TRUE, value=TRUE))
  } else if (grepl("(?i).tiff", Image_files[[2]])==TRUE){
    suppressWarnings(Image_files <- grep(list.files(path=Batch_Folder_path), pattern=c(".tiff"), invert=TRUE, value=TRUE))
  }
  for(h in 1:length(Image_files)){
    Image_file_folder <- Image_files[[h]]
    Image_folder_path <- paste0(Batch_Folder_path,Image_file_folder,"/")
    Image_csv_data <- list.files(Image_folder_path,pattern=".csv")
    main_csv_path <- paste0(Image_folder_path,Image_csv_data[[1]])
    MAIN_CSV_DATA <- read.csv(main_csv_path,header=T)
    MAIN_CSV_DATA <- MAIN_CSV_DATA %>% dplyr::select(.,x.0.s.area,
                                                     x.0.s.perimeter,
                                                     x.0.s.radius.mean)
    csv_image_list <- list(MAIN_CSV_DATA)
    csv_batch <- append(csv_batch,csv_image_list)
    rm(csv_image_list)
  }
  csv_batch <- do.call('rbind',csv_batch)
  csv_batch_list <- list(csv_batch)
  main_data2 <- append(main_data2,csv_batch_list)
  rm(csv_batch_list)
}

main_data1 <- do.call('rbind',main_data1)
main_data2 <- do.call('rbind',main_data2)

main_data <- rbind(main_data1,main_data2)

#Area
cell_area_avg <- mean(main_data$x.0.s.area,na.rm=TRUE)
cell_area_std <- as.numeric(sd(main_data$x.0.s.area,na.rm=TRUE))
cell_area_1QR <- (cell_area_avg)-(2*cell_area_std)
cell_area_3QR <- (cell_area_avg)+(2*cell_area_std)

#Perimeter
cell_peri_avg <- mean(main_data$x.0.s.perimeter,na.rm=TRUE)
cell_peri_std <- as.numeric(sd(main_data$x.0.s.perimeter,na.rm=TRUE))
cell_peri_1QR <- (cell_peri_avg)-(2*cell_peri_std)
cell_peri_3QR <- (cell_peri_avg)+(2*cell_peri_std)

#Radius
cell_radius_avg <- mean(main_data$x.0.s.radius.mean,na.rm=TRUE)
cell_radius_std <- as.numeric(sd(main_data$x.0.s.radius.mean,na.rm=TRUE))
cell_radius_1QR <- (cell_radius_avg)-(2*cell_radius_std)
cell_radius_3QR <- (cell_radius_avg)+(2*cell_radius_std)

#Compile 20X Data
Main_Shape_Data[nrow(Main_Shape_Data) + 1, ] <- c('Microcystis','20X','Area',cell_area_avg,cell_area_std,cell_area_1QR,cell_area_3QR)
Main_Shape_Data[nrow(Main_Shape_Data) + 1, ] <- c('Microcystis','20X','Perimeter',cell_peri_avg,cell_peri_std,cell_peri_1QR,cell_peri_3QR)
Main_Shape_Data[nrow(Main_Shape_Data) + 1, ] <- c('Microcystis','20X','Radius',cell_radius_avg,cell_radius_std,cell_radius_1QR,cell_radius_3QR)

rm(list = setdiff(ls(),c("Main_Shape_Data")))

save_path <- paste0(getwd(),"/")
csv_save <- paste0(paste("Cell_Features_Data"),".csv")
write.csv(Main_Shape_Data,paste0(save_path,csv_save))
