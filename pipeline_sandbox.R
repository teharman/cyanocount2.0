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
#___________________________________________________________#

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

addResourcePath(prefix = 'pics', directoryPath = 'C:/Users/Tyler.Harman/Desktop/cellcount_work')

images_list<<-data.frame(img_name = character(0))

ui_main = fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  theme = shinytheme("cosmo"),
  sidebarPanel(navlistPanel(
    widths=c(12,12),
    "CyanoSCOPE - Prediction and Enumeration UI",
    header = div("",img(src='pics/CyanoSCOPE_Logo.png',
                        id = "CyanoSCOPE Logo", height = "125px",width = "320px",style = "position: relative; margin:-15px 0px; display:right-align;"))
  ),
  h3(" "),
  h3(" "),
  actionButton("close", "Close application",class = "btn-success",style='height:35px;width:320px;font-size:140%',icon=icon("check"),style="display:center-align"),
  h3(" "),
  "Developed by NOAA NCCOS HAB-F and UCSD Qualcomm Institute"),
  mainPanel(navbarPage(
    title=h4("CyanoSCOPE Tools"),
    tabPanel(h5("1) Image(s) Selection and Upload"),
             fluidRow(
               shinyDirButton('path1','1) Select Images','Please select a folder containing images',FALSE,class = "btn-success",style='height:35px;width:175px;font-size:100%;display:center-align',icon=icon("folder-open")),
               actionButton("update1","2) Update Data",class = "btn-success",style='height:35px;width:175px;font-size:100%;display:center-align',icon=icon("arrows-rotate")),
               actionButton("run1","3) Upload Images",class = "btn-success",style='height:35px;width:175px;font-size:100%;display:center-align',icon=icon("upload")),
               actionButton("update2","4) View Images",class = "btn-success",style='height:35px;width:175px;font-size:100%;display:center-align',icon=icon("camera"))
             ),
             h6(" "),
             fluidRow(
               verbatimTextOutput('path1',placeholder = TRUE)
             ),
             fluidRow(
               column(5,
                      wellPanel(DT::dataTableOutput('img_data'),width=3,style = "font-size:70%")
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
    tabPanel(h5("2) Binary Segmentation Modeling"),
             fluidRow(
               actionButton("load1","1) Load Segmentation Model",class = 'btn-success',style='height:35px;width:225px;font-size:100%;display:center-align',icon=icon("upload")),
               actionButton("run2","2) Run Binary Segmentation",class = 'btn-success',style='height:35px;width:225px;font-size:100%;display:center-align',icon=icon('person-running'))
             ),
             h5(" "),
             fluidRow(
               column(7,
                      wellPanel(plotOutput("current_image_plot1"),style = "padding: 0px;")
                      ),
               actionButton("previous1", "Previous", icon=icon("arrow-left"),style='height:35px;width:150px;font-size:120%;display:center-align'),
               actionButton("next1", "Next", icon=icon("arrow-right"),style='height:35px;width:150px;font-size:120%;display:center-align')
             )
    ),
    tabPanel(h5("3) ID Prediction Modeling"),
             fluidRow(
               actionButton("run3","1) Run ID Prediction",class = 'btn-success',style='height:35px;width:225px;font-size:100%;display:center-align',icon=icon('magnifying-glass')),
               actionButton("update3","2) Update Predict Data",class = "btn-success",style='height:35px;width:225px;font-size:100%;display:center-align',icon=icon("arrows-rotate")),
               actionButton("update4","3) View Segmented Cells",class = "btn-success",style='height:35px;width:225px;font-size:100%;display:center-align',icon=icon("camera"))
             ),
             h6(" "),
             fluidRow(
               column(5,
                      wellPanel(DT::dataTableOutput('predict_data'),width=3,style = "font-size:70%")
               ),
               column(7,
                      wellPanel(plotOutput("current_image_plot2"),style = "padding: 0px;")
               )
             ),
             fluidRow(
               column(5,
                      textOutput("image_info"),placeholder=TRUE,style="font-size:105%;font-weight:bold"),
               column(4,
                      actionButton("previous2", "IMG_Previous", icon=icon("arrow-left"),style='height:35px;width:150px;font-size:100%;display:center-align')),
               column(3,
                      actionButton("next2", "IMG_Next", icon=icon("arrow-right"),style='height:35px;width:150px;font-size:100%;display:center-align'))
             ),
             fluidRow(
               column(2,
                      textOutput("image_num1"),placeholder=TRUE,style="font-size:105%;font-weight:bold"),
               column(3,
                      textOutput("seg_num"),placeholder=TRUE,style="font-size:105%;font-weight:bold"),
               column(4,
                      actionButton("previous3", "Cell_Previous", icon=icon("arrow-left"),style='height:35px;width:150px;font-size:100%;display:center-align')),
               column(3,
                      actionButton("next3", "Cell_Next", icon=icon("arrow-right"),style='height:35px;width:150px;font-size:100%;display:center-align'))
             )
    ),
    tabPanel(h5("4) CyanoSCOPE analysis")
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
  index <- reactiveVal(1)
  observe({
    shinyDirChoose(input,'path1',roots=volumes,session=session,filetypes=c('','txt'))
    dirname_path1<- shiny::reactive({shinyFiles::parseDirPath(volumes,input$path1)})
    shiny::observe({
      if(!is.null(dirname_path1)){
        print(dirname_path1())
        output$path1<-shiny::renderText(dirname_path1())
        img_dir<<-paste0(normalizePath(dirname_path1(),winslash = "/"),"/")
        images <<- list.files(img_dir, full.name = T)
        image_names <<- list.files(img_dir, full.name = F)
        images_list<<-read.table(text=image_names,col.names = c('img_name'))
      }
    })
  })
  rv <- reactiveVal(images_list)
  dir <- reactive(input$path1)
  output$dir <- renderText({  # use renderText instead of renderPrint
    parseDirPath(c(home = '~'), dir())
  })
  output$img_data <- DT::renderDataTable ({
    DT::datatable(rv(), editable = TRUE)
  })
  onclick("update1",{
    proxy=dataTableProxy("img_data")
    replaceData(proxy,images_list)
  })
  observeEvent(input$run1, {
    message("uploading images - please wait")
    read_images <<- lapply(images, readTIFF)
    beepr::beep(sound=1)
    message("***image upload complete***")
  })

  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1, length(images)))
  })
  observeEvent(input$update2,{
    output$current_image_plot <- renderPlot({
      loaded_image <- magick::image_ggplot(image_read(read_images[[index()]]))
      loaded_image
    },res=300,width=375,height=375)
  })

  ####server - tab 2####
  index1 <- reactiveVal(1)
  observeEvent(input[["previous1"]], {
    index1(max(index1()-1, 1))
  })
  observeEvent(input[["next1"]], {
    index1(min(index1()+1, length(mask_main)))
  })
  observeEvent(input$load1,{
    message("loading segmentation model - please wait")
    model<<-load_model_tf('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/segmentation_model/updated_test_model/', custom_objects = NULL, compile = TRUE)
    #change the file path once models are finalized and placed in the package directory
    beepr::beep(sound=1)
    message("***model upload complete***")
  })
  observeEvent(input$run2,{
    message("running segmentation model - please wait")
    test_img<-image_load(images[[1]],target_size = c(1024,1024))
    new_array<-test_img%>%image_to_array()%>%array_reshape(.,c(1,dim(.)))%>%'/'(255)
    mask_main<<-model%>%predict(new_array)%>%
      get_masks(binary_colormap)
    for (z in 2:length(read_images)){
      test_img<-image_load(images[[z]],target_size = c(1024,1024))
      new_array<-test_img%>%image_to_array()%>%array_reshape(.,c(1,dim(.)))%>%'/'(255)
      mask<-model%>%predict(new_array)%>%
        get_masks(binary_colormap)
      mask_main <<- append(mask_main, mask)
    }
    beepr::beep(sound=1)
    message("***segmentation prediction complete***")
    output$current_image_plot1 <- renderPlot({
      loaded_image1 <- magick::image_ggplot(image_read(mask_main[[index1()]]/255))
      loaded_image1
    },res=300,width=400,height=400)
  })

  ####server - tab 3####
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
  output$image_info<-renderText({
    paste('Image: ',image_names[[index2()]])
  })
  output$image_num1<-renderText({
    paste('Image #: ',index2())
  })
  output$seg_num<-renderText({
    paste('Cell #: ',index3())
  })
  observeEvent(input$run3,{
    message("running ID prediction model - please wait")
    predict_model<<-load_model_tf('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/small_data/model/', custom_objects = NULL, compile = TRUE)
    watershed_convert <- function(x, w = 17, h = 17, offset = 0.001, areathresh = 50, tolerance= 0.5, ext = 1, removeEdgeCells = TRUE) {
      if (removeEdgeCells == TRUE){
        image <- thresh(x, w = w, h = h, offset = offset)
        image1 <- fillHull(image)
        image2 <- watershed(distmap(image1), tolerance = tolerance, ext = ext)
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
        image2 <- watershed(distmap(image1), tolerance = tolerance, ext = ext)
        nf <- computeFeatures.shape(image2)
        nr <- which(nf[, "s.area"] < areathresh)
        image3 <- rmObjects(image2, nr)
        return(image3)
      }
    }
    test_img<-image_load(images[[6]],target_size = c(1024,1024))
    img_array<-test_img%>%image_to_array()%>%'/'(255)
    rgb.imgs<-Image(img_array,colormode = Color)
    mask<-abind(mask_main[[6]])
    mask<-mask[,,1]
    display(mask)
    img_watershed<-watershed_convert(mask,w=50,h=50,offset=0.001,areathresh=50,tolerance=0.5,ext = 1,removeEdgeCells=TRUE)
    ctmask<-opening(img_watershed>0.1,makeBrush(5,shape='disc'))
    seed_mask<-single_cell_convert(ctmask)
    cmask<-propagate(mask,seeds=seed_mask,mask=ctmask,lambda = 10^1)
    cmask1<-array_reshape(cmask,c(dim(cmask),1))
    display(rgb.imgs)
    display(cmask1)
    segmented<-paintObjects(cmask,rgb.imgs,col = c('black','orange'))
    display(segmented,all=TRUE)
    st_img <- stackObjects(cmask,rgb.imgs)
    st_img_test <- Image(st_img)
    cell_seg <<- list(st_img_test)
    for (z in 7:length(read_images)){
      test_img<-image_load(images[[z]],target_size = c(1024,1024))
      img_array<-test_img%>%image_to_array()%>%'/'(255)
      rgb.imgs<-Image(img_array,colormode = Color)
      mask<-abind(mask_main[[z]])
      mask<-mask[,,1]
      display(mask)
      img_watershed<-watershed_convert(mask,w=50,h=50,offset=0.001,areathresh=5,tolerance=0.5,ext = 1,removeEdgeCells=TRUE)
      ctmask<-opening(img_watershed>0.1,makeBrush(5,shape='disc'))
      seed_mask<-single_cell_convert(ctmask)
      cmask<-propagate(mask,seeds=seed_mask,mask=ctmask,lambda = 10^1)
      cmask1<-array_reshape(cmask,c(dim(cmask),1))
      display(rgb.imgs)
      display(cmask1)
      segmented<-paintObjects(cmask,rgb.imgs,col = c('black','orange'))
      display(segmented,all=TRUE)
      st_img <- stackObjects(cmask,rgb.imgs)
      seg_cell<-list(st_img)
      seg_cell_test<-Image(seg_cell)
      cell_seg<<-append(cell_seg,seg_cell_test)
    }
    beepr::beep(sound=1)
    message("cyanobacteria ID prediction complete")
  })
  observeEvent(input$update3,{
    model_label<-dir("C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/misc/Train/")
    resize_img<-resize(img_select[,,,1],w=100,h=100)
    x <- image_to_array(resize_img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- x/255
    pred <- predict_model %>% predict(x)
    pred <- data.frame("Species" = model_label, "Probability" = t(pred))
    pred <- pred[order(pred$Probability, decreasing=T),][1:2,]
    pred$Probability <- paste(format(100*pred$Probability,2),"%")
    pred_list<<-read.table(text=image_names,col.names = c('img_name'))
  })
  observeEvent(input$update4,{
    output$current_image_plot2 <- renderPlot({
      img_select<<-(cell_seg[[index2()]])
      loaded_image2 <- magick::image_ggplot(image_read(img_select[,,,index3()]))
      loaded_image2
    },res=300,width=375,height=375)
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

runGadget(ui_main, server_main, viewer = dialogViewer("CyanoSCOPE - Prediction and Enumeration UI",width = 1200, height = 800))
