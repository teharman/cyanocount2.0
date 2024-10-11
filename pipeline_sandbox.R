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
library(DescTools)
#___________________________________________________________#

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

addResourcePath(prefix = 'pics', directoryPath = '.')

ui_main = fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  theme = shinytheme("cosmo"),
  sidebarPanel(navlistPanel(
    widths=c(12,12),
    "BloomSCOPE - Automated Cyanobacteria Prediction and Enumeration Interface",
    h4(" "),
    header = div("",img(src='pics/BloomSCOPE_Logo.png',
                        id = "CyanoSCOPE Logo", height = "140px",width = "320px",style = "position: relative; margin:-15px 0px; display:right-align;"))
  ),
  h3(" "),
  h3(" "),
  actionButton("close", "Close application",class = "btn-danger",style='height:35px;width:320px;font-size:140%',icon=icon("check"),style="display:center-align"),
  h3(" "),
  "Developed by NOAA NCCOS HAB-Forecasting Branch"),
  mainPanel(navbarPage(
    title=h4("BloomSCOPE Tools"),
    tabPanel(h6("1) Image Selection and Upload"),
             fluidRow(
               shinyDirButton('path1','1) Select Images','Please select a folder containing images',FALSE,class = "btn-success",style='height:35px;width:182px;font-size:100%;display:center-align',icon=icon("folder-open")),
               actionButton("update1","2) Update Data",class = "btn-success",style='height:35px;width:182px;font-size:100%;display:center-align',icon=icon("arrows-rotate")),
               actionButton("run1","3) Upload Images",class = "btn-success",style='height:35px;width:182px;font-size:100%;display:center-align',icon=icon("upload"))
             ),
             h6(" "),
             fluidRow(style = "display: flex;",
               column(6,verbatimTextOutput('path1',placeholder = TRUE),
                      style = "font-size:70%;height:75px"),
               column(6,selectInput("selectData",
                                     h6(strong("Select objective magnification:")),
                                     choices=list("20X","40X"),selected="20X"),
                      style = "font-size:70%;height:75px")
             ),
             h6(" "),
             fluidRow(
               column(5,
                      wellPanel(DT::dataTableOutput('img_data'),width=3,style = "font-size:70%;height:500px")
               ),
               column(7,
                      wellPanel(plotOutput("current_image_plot"),style = "padding: 0px;")
               )
             ),
             fluidRow(
               column(4,offset = 5,
                      actionButton("previous", "Previous", icon=icon("arrow-left"),style='height:35px;width:150px;font-size:120%;display:center-align')),
               column(3,
                      actionButton("next", "Next", icon=icon("arrow-right"),style='height:35px;width:150px;font-size:120%;display:center-align'))
             )
    ),
    tabPanel(h6("2) ID Prediction Modeling"),
             fluidRow(
               actionButton("run3","1) Apply Segmentation",class = 'btn-success',style='height:35px;width:185px;font-size:90%;display:center-align',icon=icon('magnifying-glass')),
               actionButton("update3","2) Run ID Prediction",class = "btn-success",style='height:35px;width:185px;font-size:100%;display:center-align',icon=icon("person-running")),
               actionButton("update5","3) Update Data",class = "btn-success",style='height:35px;width:185px;font-size:100%;display:center-align',icon=icon("arrows-rotate"))
             ),
             h6(" "),
             fluidRow(
               column(9,
                      wellPanel(DT::dataTableOutput('predict_data',width = '500px'),style = "font-size:60%;height:550px;width:550px")
               ),
               column(3,
                      wellPanel(plotOutput("current_image_plot2"),style = "padding: 0px;height:150px")
               )
             ),
             h4(" "),
             h4(" "),
             fluidRow(
               column(8,
                      textOutput("image_info"),placeholder=TRUE,style="font-size:105%;font-weight:bold"),
               column(2,
                      actionButton("previous2", "IMG_Previous", icon=icon("arrow-left"),style='height:35px;width:125px;font-size:90%;display:center-align')),
               column(2,
                      actionButton("next2", "IMG_Next", icon=icon("arrow-right"),style='height:35px;width:125px;font-size:100%;display:center-align'))
             ),
             fluidRow(
               column(2,
                      textOutput("image_num1"),placeholder=TRUE,style="font-size:105%;font-weight:bold"),
               column(6,
                      textOutput("seg_num"),placeholder=TRUE,style="font-size:105%;font-weight:bold"),
               column(2,
                      actionButton("previous3", "Cell_Previous", icon=icon("arrow-left"),style='height:35px;width:125px;font-size:90%;display:center-align')),
               column(2,
                      actionButton("next3", "Cell_Next", icon=icon("arrow-right"),style='height:35px;width:125px;font-size:100%;display:center-align'))
             )
    ),
    tabPanel(h6("3) BloomSCOPE analysis"),
             fluidRow(
               actionButton("run4","1) Generate Predict Images",class = 'btn-success',style='height:35px;width:180px;font-size:80%;display:center-align',icon=icon('wrench')),
               shinyDirButton('path2','2) Select Save Directory','Please select a folder to export data/images',FALSE,class = "btn-success",style='height:35px;width:180px;font-size:80%;display:center-align',icon=icon("folder-open")),
               actionButton("save1","3) Save Images/Datasets",class = 'btn-success',style='height:35px;width:180px;font-size:90%;display:center-align',icon=icon('save'))
             ),
             h6(" "),
             fluidRow(
               verbatimTextOutput('path2',placeholder = TRUE)
             ),
             h6(" "),
             column(12,
                    wellPanel(plotOutput("current_image_plot3"),style = "padding: 0px;height:500px")
             ),
             fluidRow(
               column(7,offset=1,
                      actionButton("previous4", "Previous", icon=icon("arrow-left"),style='height:35px;width:125px;font-size:100%;display:center-align')),
               column(3,offset=1,
                      actionButton("next4", "Next", icon=icon("arrow-right"),style='height:35px;width:125px;font-size:100%;display:center-align'))
             )
    ),
    tags$style(type = 'text/css', '.navbar { background-color: #303030;
                           font-family: Arial;
                           font-size: 13px;
                           color: #C2C2C2; }',
               '.navbar-default .navbar-brand {
                             color: #C8C8C8;
                             font-size:17px;
                           }')
  ))
)

server_main = function(input, output, session) {
  volumes=getVolumes()()
  ####server - tab 1####
  images_list <<- data.frame(image_names = character(0))
  index <- reactiveVal(1)
  observe({
    shinyDirChoose(input,'path1',roots=volumes,session=session,filetypes=c('','txt'))
    dirname_path1 <- shiny::reactive({shinyFiles::parseDirPath(volumes,input$path1)})
    shiny::observe({
      if(!is.null(dirname_path1)){
        print(dirname_path1())
        output$path1 <- shiny::renderText(dirname_path1())
        img_dir <<- paste0(normalizePath(dirname_path1(),winslash = "/"),"/")
        images <<- list.files(img_dir, full.name = T)
        image_names <<- list.files(img_dir, full.name = F)
        images_list <<- data.frame(image_names)
      }
    })
  })

  observeEvent(input$selectData, {
    if(input$selectData == '20X'){
      analysis_type<<-("20X")
    }
    else if(input$selectData == "20X"){
      analysis_type<<-("20X")
    }
  })

  rv <- reactiveVal(images_list)
  dir <- reactive(input$path1)
  output$dir <- renderText({
    parseDirPath(c(home = '~'), dir())
  })
  output$img_data <- DT::renderDataTable ({
    DT::datatable(rv(), editable = TRUE)
  })
  onclick("update1",{
    proxy=dataTableProxy("img_data")
    replaceData(proxy,images_list)
  })

  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1, length(images)))
  })

  observeEvent(input$run1, {
    shinyCatch({message("uploading images - please wait")}, prefix = '', position = "bottom-left")
    Sys.sleep(3)
    read_images <<- lapply(images, EBImage::readImage)
    shinyCatch({message("***image upload complete***")}, prefix = '', position = "bottom-left")
    output$current_image_plot <- renderPlot({
      loaded_image <- magick::image_ggplot(image_read(read_images[[index()]]))
      loaded_image
    },res=300,width=400,height=400)
    beepr::beep(sound=1)
  })

  #observeEvent(input[["previous"]], {
  #  index(max(index()-1, 1))
  #})
  #observeEvent(input[["next"]], {
  #  index(min(index()+1, length(images)))
  #})
  #observeEvent(input$update2,{
  #  output$current_image_plot <- renderPlot({
  #    loaded_image <- magick::image_ggplot(image_read(read_images[[index()]]))
  #    loaded_image
  #  },res=300,width=400,height=400)
  #})

  ####server - tab 2####
  total_results <<- data.frame(file_ID = character(0), cell_number = numeric(0), Shape.Estimate = character(0), Shape.Percent = numeric(0), ID.Estimate = character(0), ID.Percent = numeric(0))
  shape.input <<- data.frame(file_ID = character(0), cell_number = numeric(0), Shape.Estimate = character(0), Shape.Percent = numeric(0))
  ID.input <<- data.frame(file_ID = character(0), cell_number = numeric(0), ID.Estimate = character(0), ID.Percent = numeric(0))
  index2 <- reactiveVal(1)
  index3 <- reactiveVal(1)
  observeEvent(input[["previous2"]], {
    index2(max(index2()-1, 1))
  })
  observeEvent(input[["next2"]], {
    index2(min(index2()+1, length(cell_seg)))
  })
  observeEvent(input[["previous3"]], {
    index3(max(index3()-1, 1))
  })
  observeEvent(input[["next3"]], {
    index3(min(index3()+1, dim(img_select)[4]))
  })
  output$image_info <- renderText({
    paste('Image: ',image_names[[index2()]])
  })
  output$image_num1 <- renderText({
    paste('Image #: ',index2())
  })
  output$seg_num <- renderText({
    paste('Cell #: ',index3())
  })

  observeEvent(input$run3,{
    cell.coord <<- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0))
    cell.count <<- data.frame(image_name = character(0), Microcystis_count = numeric(0), Anabaena_count = numeric(0), Dolichospermum_count = numeric(0))
    shinyCatch({message("loading segmentation model - please wait")}, prefix = '', position = "bottom-left")
    Sys.sleep(3)
    model <<- load_model_tf('./models/binary_segmentation/updated_test_model_7/', custom_objects = NULL, compile = TRUE)
    #change the file path once models are finalized and placed in the package directory
    shinyCatch({message("***model upload complete***")}, prefix = '', position = "bottom-left")

    Sys.sleep(3)
    shinyCatch({message("running segmentation model - please wait")}, prefix = '', position = "bottom-left")
    Sys.sleep(3)

    test_img <- image_load(images[[1]],target_size = c(1024,1024))
    new_array <- test_img %>% image_to_array() %>% array_reshape(.,c(1,dim(.))) %>% '/'(255)
    mask_main <<- model %>% predict(new_array) %>%
      get_masks(binary_colormap)
    for (z in 2:length(read_images)){
      test_img <- image_load(images[[z]],target_size = c(1024,1024))
      new_array <- test_img %>% image_to_array() %>% array_reshape(.,c(1,dim(.))) %>% '/'(255)
      mask <- model %>% predict(new_array) %>%
        get_masks(binary_colormap)
      mask_main <<- append(mask_main, mask)
    }
    shinyCatch({message("***segmentation prediction complete***")}, prefix = '', position = "bottom-left")

    Sys.sleep(3)
    shinyCatch({message("applying binary segmentation - please wait")}, prefix = '', position = "bottom-left")
    Sys.sleep(3)

    shape_model <<- load_model_tf('./models/shape_model/shape_model01/', custom_objects = NULL, compile = TRUE)
    predict_model <<- load_model_tf('./models/ID_prediction/ID_model02_mod/', custom_objects = NULL, compile = TRUE)
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
    EBImage::display(rgb.imgs)
    mask <- abind(mask_main[[1]])
    mask <- mask[,,1]
    EBImage::display(mask)
    img_watershed <- watershed_convert(mask,w=25,h=25,offset=0.001,areathresh=100,tolerance=0.6,ext = 4,removeEdgeCells=TRUE)
    EBImage::display(img_watershed)
    final_img <- count_images(img_watershed,normalize = T, removeEdgeCells = T)
    EBImage::display(final_img)
    count_cells(img_watershed)
    ctmask <- opening(img_watershed>0.1,makeBrush(5,shape='disc'))
    seed_mask <- single_cell_convert(ctmask,w=25,h=25,offset=0.001,areathresh=100,tolerance=0.6,ext = 4)
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
    cell_seg <<- list(st_img_test)
    blob_seg <<- list(st_img_blob)
    coord.mtx <- RSAGA::grid.to.xyz(cmask)
    filter <- filter(coord.mtx,z>0)
    for(u in 1:dim(st_img_test)[4]){
      filter.mtx <- subset(filter,z==u)
      xmin <- min(filter.mtx$x)-1
      xmax <- max(filter.mtx$x)+1
      ymin <- min(filter.mtx$y)-1
      ymax <- max(filter.mtx$y)+1
      cell.coord[nrow(cell.coord) + 1, ] <<- c(xmin, xmax, ymin, ymax)
      rm(filter.mtx)
    }
    rm(coord.mtx)
    rm(filter)

    shape.features.data <<- read.csv('Cell_Features_Data.csv',header=T)

    features.data.img <<- computeFeatures.shape(cmask,rgb.imgs)
    features.data.img <<- as.data.frame(features.data.img)
    features.data.img$file_ID <<- image_names[[1]]
    features.data.img <<- features.data.img %>% dplyr::select(.,file_ID,
                                                             s.area,
                                                             s.perimeter,
                                                             s.radius.mean)
    features.data.img$cell_number <<- 1:nrow(features.data.img)

    for (z in 2:length(read_images)){
      test_img <- image_load(images[[z]],target_size = c(1024,1024))
      img_array <- test_img %>% image_to_array() %>% '/'(255)
      rgb.imgs <- Image(img_array,colormode = Color)
      mask <- abind(mask_main[[z]])
      mask <- mask[,,1]
      img_watershed <- watershed_convert(mask,w=25,h=25,offset=0.001,areathresh=100,tolerance=0.6,ext = 4,removeEdgeCells=TRUE)
      ctmask <- opening(img_watershed>0.1,makeBrush(5,shape='disc'))
      seed_mask <- single_cell_convert(ctmask,w=25,h=25,offset=0.001,areathresh=100,tolerance=0.6,ext = 4)
      cmask <- propagate(mask,seeds=seed_mask,mask=ctmask,lambda = 10^1)
      cmask1 <- array_reshape(cmask,c(dim(cmask),1))
      segmented <- paintObjects(cmask,rgb.imgs,col = c('black','orange'))
      st_img <- stackObjects(cmask,rgb.imgs)
      st_blob <- stackObjects(cmask,img_watershed)
      st_img_test <- Image(st_img)
      st_img_blob <- Image(st_blob)
      EBImage::display(st_img_blob)
      seg_cell <- list(st_img)
      seg_blob <- list(st_blob)
      seg_cell_test <- Image(seg_cell)
      seg_blob_test <- Image(seg_blob)
      cell_seg <<- append(cell_seg,seg_cell_test)
      blob_seg <<- append(blob_seg,seg_blob_test)
      coord.mtx <- RSAGA::grid.to.xyz(cmask)
      filter <- filter(coord.mtx,z>0)
      for(u in 1:dim(st_img_test)[4]){
        filter.mtx <- subset(filter,z==u)
        xmin <- min(filter.mtx$x)-1
        xmax <- max(filter.mtx$x)+1
        ymin <- min(filter.mtx$y)-1
        ymax <- max(filter.mtx$y)+1
        cell.coord[nrow(cell.coord) + 1, ] <<- c(xmin, xmax, ymin, ymax)
        rm(filter.mtx)
      }
      rm(coord.mtx)
      rm(filter)

      features.data.img1 <<- computeFeatures.shape(cmask,rgb.imgs)
      features.data.img1 <<- as.data.frame(features.data.img1)
      features.data.img1$file_ID <<- image_names[[z]]
      features.data.img1 <<- features.data.img1 %>% dplyr::select(.,file_ID,
                                                                 s.area,
                                                                 s.perimeter,
                                                                 s.radius.mean)
      features.data.img1$cell_number <<- 1:nrow(features.data.img1)
      features.data.img <<- rbind(features.data.img,features.data.img1)
      rm(features.data.img1)
    }

    shinyCatch({message("***segmentation applied***")}, prefix = '', position = "bottom-left")

    output$current_image_plot2 <- renderPlot({
      img_select <<- (cell_seg[[index2()]])
      loaded_image2 <- magick::image_ggplot(image_read(img_select[,,,index3()]))
      loaded_image2
    },res=300,width=150,height=150)

    beepr::beep(sound=1)
  })

  observeEvent(input$update3,{
    shinyCatch({message("running models - please wait")}, prefix = '', position = "bottom-left")
    sigfig <- function(vec, n=3){
      ### function to round values to N significant digits
      # input:   vec       vector of numeric
      #          n         integer is the required sigfig
      # output:  outvec    vector of numeric rounded to N sigfig

      formatC(signif(vec,digits=n))

    }

    Sys.sleep(3)
    shinyCatch({message("applying shape-based model - please wait")}, prefix = '', position = "bottom-left")
    Sys.sleep(3)

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
        model_label<-c("Microcystis","Dolichospermum")
        pred <- data.frame("Species" = model_label, "Probability" = t(pred))
        pred <- pred[order(pred$Probability, decreasing=T),][1:3,]
        pred$Probability <- (100*pred$Probability)
        pred$Probability <- paste(formatC(pred$Probability, digits = 3, format = "f"),"%")
        shape.input[nrow(shape.input) + 1, ] <<- c(file_ID, cell_num, pred$Species[[1]], pred$Probability[[1]])
      }
    }

    Sys.sleep(3)
    shinyCatch({message("applying ID prediction model - please wait")}, prefix = '', position = "bottom-left")
    Sys.sleep(3)

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
        model_label<-c("Microcystis","Dolichospermum")
        pred <- data.frame("Species" = model_label, "Probability" = t(pred))
        pred <- pred[order(pred$Probability, decreasing=T),][1:3,]
        pred$Probability <- (100*pred$Probability)
        pred$Probability <- paste(formatC(pred$Probability, digits = 3, format = "f"),"%")
        ID.input[nrow(ID.input) + 1, ] <<- c(file_ID, cell_num, pred$Species[[1]], pred$Probability[[1]])
      }
    }
    total_results <<- merge(shape.input,ID.input,by=c("file_ID","cell_number"))



    shinyCatch({message("***ID prediction complete***")}, prefix = '', position = "bottom-left")
    beepr::beep(sound=1)
  })

  rv1 <- reactiveVal(total_results)
  onclick("update5",{
    proxy=dataTableProxy("predict_data")
    replaceData(proxy,total_results)
  })
  output$predict_data <- DT::renderDataTable ({
    DT::datatable(rv1(), editable = TRUE,    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(targets=c(0), visible=TRUE, width='50'),
                        list(targets=c(1), visible=TRUE, width='50'),
                        list(targets=c(2), visible=TRUE, width='50'),
                        list(targets=c(3), visible=TRUE, width='50'),
                        list(targets=c(4), visible=TRUE, width='50'),
                        list(targets=c(5), visible=TRUE, width='50'),
                        list(targets='_all', visible=FALSE)
      )))
  })

  ####server - tab 3####
  #cell.coord <<- data.frame(xmin = numeric(0), xmax = numeric(0), ymin = numeric(0), ymax = numeric(0))
  #cell.count <<- data.frame(image_name = character(0), Microcystis_count = numeric(0), Anabaena_count = numeric(0), Dolichospermum_count = numeric(0))
  index5 <- reactiveVal(1)
  observeEvent(input[["previous4"]], {
    index5(max(index5()-1, 1))
  })
  observeEvent(input[["next4"]], {
    index5(min(index5()+1, length(images)))
  })
  observeEvent(input$run4,{
    shinyCatch({message("generating prediction images - please wait")}, prefix = '', position = "bottom-left")

    prediction_labels <<- data.frame(prediction_results = character(0))
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
        prediction_labels[nrow(prediction_labels) + 1, ] <<- c(value1)
      })
      try(if(predict1 != predict2 & value_result == FALSE){
        value3 <- "ESTIMATE"
        prediction_labels[nrow(prediction_labels) + 1, ] <<- c(value3)
      })
      try(if(predict1 == predict2 & value_result == TRUE){
        value3 <- "ESTIMATE"
        prediction_labels[nrow(prediction_labels) + 1, ] <<- c(value3)
      })
      try(if(predict1 != predict2 & value_result == TRUE){
        value4 <- "NEGATIVE"
        prediction_labels[nrow(prediction_labels) + 1, ] <<- c(value4)
      })
    }

    total_results <<- cbind(total_results,prediction_labels)

    total_results <<- merge(total_results,features.data.img,by=c("file_ID","cell_number"))
    total_results$cell_number <<- as.character(total_results$cell_number)
    total_results <<- total_results %>%
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
    total_results <<- total_results[order(total_results$cell_number,decreasing=FALSE),]
    total_results <<- total_results[order(total_results$file_ID,decreasing=FALSE),]

    total_results <<- cbind(total_results,cell.coord)

    new_shape.features.data <<- subset(shape.features.data,Objective==analysis_type)

    for(y in 1:dim(total_results)[1]){
      cell_row_data <<- total_results[y,]
      if(cell_row_data$ID.Estimate == "Microcystis"){
        if(cell_row_data$s.area %][% c(new_shape.features.data$SD_Minus2[4],new_shape.features.data$SD_Plus2[4])==TRUE){
          total_results$ID.Estimate[y] <<- "UNKNOWN"
          total_results$prediction_results[y] <<- "POSITIVE"
        } else{}
      } else if(cell_row_data$ID.Estimate == "Dolichospermum"){
        if(cell_row_data$s.area %][% c(new_shape.features.data$SD_Minus2[1],new_shape.features.data$SD_Plus2[1])==TRUE){
          total_results$ID.Estimate[y] <<- "UNKNOWN"
          total_results$prediction_results[y] <<- "POSITIVE"
        } else{}
      }
    }

   #watershed_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1, removeEdgeCells = TRUE) {
   #  if (removeEdgeCells == TRUE){
   #    image <- thresh(x, w = w, h = h, offset = offset)
   #    image1 <- fillHull(image)
   #    image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
   #    nf <- computeFeatures.shape(image2)
   #    nr <- which(nf[, "s.area"] < areathresh)
   #    image3 <- rmObjects(image2, nr)
   #    dims <- dim(image3)
   #    border1 <- c(image3[1:dims[1], 1], image3[1:dims[1], dims[2]], image3[1, 1:dims[2]], image3[dims[1], 1:dims[2]])
   #    ids <- unique(border1[which(border1 != 0)])
   #    inner <- rmObjects(image3, ids)
   #    return(inner)
   #  } else{
   #    image <- thresh(x, w = w, h = h, offset = offset)
   #    image1 <- fillHull(image)
   #    image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
   #    nf <- computeFeatures.shape(image2)
   #    nr <- which(nf[, "s.area"] < areathresh)
   #    image3 <- rmObjects(image2, nr)
   #    return(image3)
   #  }
   #}
   #single_cell_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1) {
   #  image <- thresh(x, w = w, h = h, offset = offset)
   #  image1 <- fillHull(image)
   #  image2 <- EBImage::watershed(distmap(image1), tolerance = tolerance, ext = ext)
   #  nf <- computeFeatures.shape(image2)
   #  nr <- which(nf[, "s.area"] < areathresh)
   #  image3 <- rmObjects(image2, nr)
   #  return(image3)
   #}
   #for(k in 1:length(images)) {
   #  cell_img <- cell_seg[[k]]
   #  test_img <- image_load(images[[k]],target_size = c(1024,1024))
   #  img_array <- test_img %>% image_to_array() %>% '/'(255)
   #  rgb.imgs <- Image(img_array,colormode = Color)
   #  mask <- abind(mask_main[[k]])
   #  mask <- mask[,,1]
   #  display(mask)
   #  img_watershed <- watershed_convert(mask,w=25,h=25,offset=0.001,areathresh=100,tolerance=0.6,ext = 4,removeEdgeCells=TRUE)
   #  final_img <- count_images(img_watershed,normalize = T, removeEdgeCells = T)
   #  EBImage::display(final_img)
   #  ctmask <- opening(img_watershed>0.1,makeBrush(5,shape='disc'))
   #  seed_mask <- single_cell_convert(ctmask)
   #  display(seed_mask)
   #  cmask <- propagate(mask,seeds=seed_mask,mask=ctmask,lambda = 10^1)
   #  segmented <- paintObjects(cmask,rgb.imgs,col = c('black','orange'))
   #  display(segmented)
   #  coord.mtx <- RSAGA::grid.to.xyz(cmask)
   #  filter <- filter(coord.mtx,z>0)
   #  for(u in 1:dim(cell_img)[4]){
   #    filter.mtx <- subset(filter,z==u)
   #    xmin <- min(filter.mtx$x)-1
   #    xmax <- max(filter.mtx$x)+1
   #    ymin <- min(filter.mtx$y)-1
   #    ymax <- max(filter.mtx$y)+1
   #    cell.coord[nrow(cell.coord) + 1, ] <- c(xmin, xmax, ymin, ymax)
   #    rm(filter.mtx)
   #  }
   #  rm(coord.mtx)
   #  rm(filter)
   #}
   #total_results <<- cbind(total_results,cell.coord)

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

    estimate_plot <- test_img1+geom_rect(data = test_ID.input,
                                         aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax,
                                             fill = ID.Estimate, colour = ID.Estimate,linetype = prediction_results),
                                         alpha = .20, linewidth = 0.15, inherit.aes = FALSE)+
      scale_colour_manual(name="ID Prediction",
                          values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
      scale_fill_manual(name="ID Prediction",
                        values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
      scale_linetype_manual(name="Prediction Results",
                            labels = c("Estimate", "Negative", "Positive"),
                            values = c(2,3,1))+
      guides(linetype = guide_legend(override.aes = list(
        linetype = c("dashed", "dotted", "solid"),
        color = c("black","black", "black"),
        fill = c(NA,NA,NA)))) +
      theme(legend.text = element_text(size=3),
            legend.title = element_text(size=3),
            legend.key.size = unit(0.25,"cm"),
            legend.spacing.y = unit(1, 'mm'),
            plot.margin=unit(c(0,0.35,0,0),"cm"),
            #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
            #                          colour = c("black","black","black",NA)),
            legend.position = "right")

    estimate_list <<- list(estimate_plot)

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

      estimate_plot <- test_img1+geom_rect(data = test_ID.input,
                                           aes(xmin = xmin, xmax = xmax,
                                               ymin = ymin, ymax = ymax,
                                               fill = ID.Estimate, colour = ID.Estimate, linetype = prediction_results),
                                           alpha = .20, linewidth = 0.15, inherit.aes = FALSE)+
        scale_colour_manual(name="ID Prediction",
                            values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
        scale_fill_manual(name="ID Prediction",
                          values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
        scale_linetype_manual(name="Prediction Results",
                              labels = c("Estimate", "Negative", "Positive"),
                              values = c(2,3,1))+
        guides(linetype = guide_legend(override.aes = list(
          linetype = c("dashed", "dotted", "solid"),
          color = c("black","black", "black"),
          fill = c(NA,NA,NA)))) +
        theme(legend.text = element_text(size=3),
              legend.title = element_text(size=3),
              legend.key.size = unit(0.25,"cm"),
              legend.spacing.y = unit(1, 'mm'),
              plot.margin=unit(c(0,0.35,0,0),"cm"),
              #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
              #                          colour = c("black","black","black",NA)),
              legend.position = "right")

      estimate_list1 <- list(estimate_plot)

      estimate_list <<- append(estimate_list,estimate_list1)
    }
    shinyCatch({message("***prediction images generated***")}, prefix = '', position = "bottom-left")

    output$current_image_plot3 <- renderPlot({
      loaded_image4 <- plot(estimate_list[[index5()]])
      loaded_image4
    },res=300,width=670,height=470)

    beepr::beep(sound=1)
  })

  observe({
    shinyDirChoose(input,'path2',roots=volumes,session=session,filetypes=c('','txt'))
    dirname_path2 <- shiny::reactive({shinyFiles::parseDirPath(volumes,input$path2)})
    shiny::observe({
      if(!is.null(dirname_path2)){
        print(dirname_path2())
        output$path2 <- shiny::renderText(dirname_path2())
        img_dir2 <<- paste0(normalizePath(dirname_path2(),winslash = "/"),"/")
      }
    })
  })
  dir2 <- reactive(input$path2)
  output$dir2 <- renderText({
    parseDirPath(c(home = '~'), dir2())
  })
  observeEvent(input$save1,{
    shinyCatch({message("saving data/images - please wait")}, prefix = '', position = "bottom-left")
    if(grepl("(?i).jpg", image_names[[2]])==TRUE){
      for (h in 1:length(cell_seg)) {
        export_cells<-cell_seg[[h]]
        Index_grey<-paste0(sub(".jpg", replacement = "_", x=image_names[[h]]))
        Index1<-paste0(sub(".jpg", replacement = "", x=image_names[[h]]))
        newpath<-paste0(img_dir2,Index1,"/")
        if(!dir.exists(newpath)){
          dir.create(newpath)
        }
        for(k in 1:dim(export_cells)[4]) {
          st_imgs_color<-export_cells[, , , k]
          analyzed_image1<-paste0(sub(".jpg", replacement = " ", x=image_names[[h]]),"_cell_")
          analyzed_image2<-paste0(sub(".jpg", replacement = " ", x=analyzed_image1),k)
          analyzed_image3<-paste0(sub(".jpg", replacement = " ", x=analyzed_image2),".png")
          writeImage(st_imgs_color,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
        }
        csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
        export_ID.input <- subset(total_results,file_ID==image_names[[h]])
        export_ID.input <- export_ID.input[,-c(8:11)]
        write.csv(export_ID.input, paste0(newpath, csv_save)) #Change this CSV file name
        rm(export_ID.input)

        #count_ID.input <- subset(ID.input,file_ID==image_names[[h]])
        #microcystis_freq<-colSums(count_ID.input=='Microcystis')
        #microcystis_freq<-microcystis_freq[[3]]
        #dolicho_freq<-colSums(count_ID.input=='Dolichospermum')
        #dolicho_freq<-dolicho_freq[[3]]
        #anabaena_freq<-colSums(count_ID.input=='Anabaena')
        #anabaena_freq<-anabaena_freq[[3]]
        #cell.count[nrow(cell.count) + 1, ] <<- c(image_names[[h]], microcystis_freq, anabaena_freq, dolicho_freq)
        #rm(count_ID.input)
        #rm(microcystis_freq)
        #rm(anabaena_freq)
        #rm(dolichospermum_freq)

        test_img <- image_load(images[[h]],target_size = c(1024,1024))
        img_array <- test_img %>% image_to_array() %>% '/'(255)
        rgb.imgs <- Image(img_array,colormode = Color)
        ex_ID.input1 <- subset(total_results,file_ID==image_names[[h]])
        test_img1 <- magick::image_ggplot(image_read(EBImage::flop(EBImage::rotate(rgb.imgs,90))))

        #estimate_plot <- test_img1+geom_rect(data = ex_ID.input1,
        #                                     aes(xmin = xmin, xmax = xmax,
        #                                         ymin = ymin, ymax = ymax,
        #                                         fill = ID.Estimate, colour = ID.Estimate),
        #                                     alpha = .20, linewidth = 0.5, inherit.aes = FALSE)+
        #  theme(legend.position = "bottom")

        estimate_plot <- test_img1+geom_rect(data = ex_ID.input1,
                                             aes(xmin = xmin, xmax = xmax,
                                                 ymin = ymin, ymax = ymax,
                                                 fill = ID.Estimate, colour = ID.Estimate, linetype = prediction_results),
                                             alpha = .20, linewidth = 0.5, inherit.aes = FALSE)+
          scale_colour_manual(name="ID Prediction",
                              values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
          scale_fill_manual(name="ID Prediction",
                            values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
          scale_linetype_manual(name="Prediction Results",
                                labels = c("Estimate", "Negative", "Positive"),
                                values = c(2,3,1))+
          guides(linetype = guide_legend(override.aes = list(
            linetype = c("dashed", "dotted", "solid"),
            color = c("black","black", "black"),
            fill = c(NA,NA,NA)))) +
          theme(legend.text = element_text(size=10),
                legend.title = element_text(size=10),
                legend.key.size = unit(0.75,"cm"),
                legend.spacing.y = unit(5, 'mm'),
                #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
                #                          colour = c("black","black","black",NA)),
                legend.position = "bottom")

        export_image<-paste0(sub(".jpg", replacement = "", x=image_names[[h]]),"_predict.png")
        ggsave(estimate_plot,filename = c(export_image),path=newpath,device = "png")
      }
    } else if (grepl("(?i).tif", image_names[[2]])==TRUE){
      for (h in 1:length(cell_seg)) {
        export_cells<-cell_seg[[h]]
        Index_grey<-paste0(sub(".tif", replacement = "_", x=image_names[[h]]))
        Index1<-paste0(sub(".tif", replacement = "", x=image_names[[h]]))
        newpath<-paste0(img_dir2,Index1,"/")
        if(!dir.exists(newpath)){
          dir.create(newpath)
        }
        for(k in 1:dim(export_cells)[4]) {
          st_imgs_color<-export_cells[, , , k]
          analyzed_image1<-paste0(sub(".tif", replacement = " ", x=image_names[[h]]),"_cell_")
          analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
          analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),".png")
          writeImage(st_imgs_color,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
        }
        csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
        export_ID.input <- subset(total_results,file_ID==image_names[[h]])
        export_ID.input <- export_ID.input[,-c(8:11)]
        write.csv(export_ID.input, paste0(newpath, csv_save)) #Change this CSV file name
        rm(export_ID.input)

        #count_ID.input <- subset(ID.input,file_ID==image_names[[h]])
        #microcystis_freq<-colSums(count_ID.input=='Microcystis')
        #microcystis_freq<-microcystis_freq[[3]]
        #dolicho_freq<-colSums(count_ID.input=='Dolichospermum')
        #dolicho_freq<-dolicho_freq[[3]]
        #anabaena_freq<-colSums(count_ID.input=='Anabaena')
        #anabaena_freq<-anabaena_freq[[3]]
        #cell.count[nrow(cell.count) + 1, ] <<- c(image_names[[h]], microcystis_freq, anabaena_freq, dolicho_freq)
        #rm(count_ID.input)
        #rm(microcystis_freq)
        #rm(anabaena_freq)
        #rm(dolichospermum_freq)

        test_img <- image_load(images[[h]],target_size = c(1024,1024))
        img_array <- test_img %>% image_to_array() %>% '/'(255)
        rgb.imgs <- Image(img_array,colormode = Color)
        ex_ID.input1 <- subset(total_results,file_ID==image_names[[h]])
        test_img1 <- magick::image_ggplot(image_read(EBImage::flop(EBImage::rotate(rgb.imgs,90))))

        #estimate_plot <- test_img1+geom_rect(data = ex_ID.input1,
        #                                     aes(xmin = xmin, xmax = xmax,
        #                                         ymin = ymin, ymax = ymax,
        #                                         fill = ID_estimate, colour = ID_estimate),
        #                                     alpha = .20, linewidth = 0.5, inherit.aes = FALSE)+
        #  theme(legend.position = "bottom")

        estimate_plot <- test_img1+geom_rect(data = ex_ID.input1,
                                             aes(xmin = xmin, xmax = xmax,
                                                 ymin = ymin, ymax = ymax,
                                                 fill = ID.Estimate, colour = ID.Estimate, linetype = prediction_results),
                                             alpha = .20, linewidth = 0.5, inherit.aes = FALSE)+
          scale_colour_manual(name="ID Prediction",
                              values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
          scale_fill_manual(name="ID Prediction",
                            values = c('Dolichospermum' = "royalblue",'Microcystis' = "magenta",'UNKNOWN' = "black"))+
          scale_linetype_manual(name="Prediction Results",
                                labels = c("Estimate", "Negative", "Positive"),
                                values = c(2,3,1))+
          guides(linetype = guide_legend(override.aes = list(
            linetype = c("dashed", "dotted", "solid"),
            color = c("black","black", "black"),
            fill = c(NA,NA,NA)))) +
          theme(legend.text = element_text(size=10),
                legend.title = element_text(size=10),
                legend.key.size = unit(0.75,"cm"),
                legend.spacing.y = unit(5, 'mm'),
                #legend.key = element_rect(linetype = c("solid","dashed","dotted","blank"),
                #                          colour = c("black","black","black",NA)),
                legend.position = "bottom")

        export_image<-paste0(sub(".tif", replacement = "", x=image_names[[h]]),"_predict.png")
        ggsave(estimate_plot,filename = c(export_image),path=newpath,device = "png")
      }
    }
    shinyCatch({message("***data and images saved***")}, prefix = '', position = "bottom-left")
    beepr::beep(sound=1)
  })

  ####other####
  observeEvent(input$close, {
    js$closeWindow()
    shinyjs::stopApp()
  })
  session$onSessionEnded(function() {
    stopApp()
  })
}

runGadget(ui_main, server_main, viewer = dialogViewer("BloomSCOPE - Automated Cyanobacteria Prediction and Enumeration UI",width = 1200, height = 800))
