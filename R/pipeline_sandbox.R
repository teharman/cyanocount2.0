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
library(cyanocount2.0)
#___________________________________________________________#

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

addResourcePath(prefix = 'pics', directoryPath = 'C:/Users/Tyler.Harman/Desktop/cellcount_work')

images_list<<-data.frame(img_name = character(0))

ui_main = fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  sidebarPanel(navlistPanel(
    widths=c(12,12),
    "CyanoSCOPE - Prediction and Enumeration UI",
    header = div("",img(src='pics/CyanoSCOPE_Logo.png',
                        id = "CyanoSCOPE Logo", height = "175px",width = "450px",style = "position: relative; margin:-15px 0px; display:right-align;"))
  ),
  h3(" "),
  h3(" "),
  actionButton("close", "Close application",class = "btn-success",style='height:35px;width:450px;font-size:140%',icon=icon("check"),style="display:center-align"),
  h3(" "),
  "Developed by NOAA NCCOS HAB-F and UCSD Qualcomm Institute"),
  mainPanel(navbarPage(
    title=h4("CyanoSCOPE Tools"),
    tabPanel(h6("1) Image(s) Selection and Upload"),
             shinyDirButton('path1','1) Select Images','Please select a folder containing images',FALSE,class = "btn-success",style='height:35px;width:150px;font-size:100%',icon=icon("folder-open")),
             actionButton("update1","2) Update Data",class = "btn-success",style='height:35px;width:150px;font-size:100%',icon=icon("arrows-rotate")),
             actionButton("run1","3) Upload Images",class = "btn-success",style='height:35px;width:150px;font-size:100%',icon=icon("upload")),
             actionButton("update2","4) View Images",class = "btn-success",style='height:35px;width:150px;font-size:100%',icon=icon("camera")),
             verbatimTextOutput('path1',placeholder = TRUE),
             column(
               DT::dataTableOutput('img_data'),width=3),
             plotOutput("current_image_plot"),
             column(3,offset = 2,
                    actionButton("previous", "Previous", icon=icon("arrow-left"),style='height:35px;width:175px;font-size:120%')),
             column(3,offset = 1,
                    actionButton("next", "Next", icon=icon("arrow-right"),style='height:35px;width:175px;font-size:120%'))
    ),
    tabPanel(h6("2) Binary Segmentation Modeling"),
             actionButton("load1","1) Load Segmentation Model",class = 'btn-success',style='height:35px;width:225px;font-size:100%',icon=icon("upload")),
             actionButton("run2","2) Run Binary Segmentation",class = 'btn-success',style='height:35px;width:225px;font-size:100%',icon=icon('person-running')),
             actionButton("previous", "Previous", icon=icon("arrow-left"),style='height:35px;width:175px;font-size:120%'),
             actionButton("next", "Next", icon=icon("arrow-right"),style='height:35px;width:175px;font-size:120%'),
             column(4,offset = 2,
                    plotOutput("current_image_plot1"))
    ),
    tabPanel(h6("3) ID Prediction Modeling"),
             actionButton("run3","Run ID Prediction",class = 'btn-success',style='height:35px;width:225px;font-size:100%',icon=icon('magnifying-glass')),
             column(4,offset = 2,
                    plotOutput("current_image_plot2")),
             column(3,offset = 2,
                    actionButton("previous", "Previous", icon=icon("arrow-left"),style='height:35px;width:175px;font-size:120%')),
             column(3,offset = 1,
                    actionButton("next", "Next", icon=icon("arrow-right"),style='height:35px;width:175px;font-size:120%')),
    ),
    tabPanel(h6("4) CyanoSCOPE analysis")
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
    beepr::beep(sound=5)
    message("***image upload complete***")
  })

  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1))
  })
  observeEvent(input$update2,{
    output$current_image_plot <- renderPlot({
      loaded_image <- magick::image_ggplot(image_read(read_images[[index()]]))
      loaded_image
    },res=300,width=375,height=375)
  })

  ####server - tab 2####
  observeEvent(input$load1,{
    message("loading segmentation model - please wait")
    model<<-load_model_tf('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_imgs/AccuScope/Draft_Model/models/segmentation_model/initial_test_model/', custom_objects = NULL, compile = TRUE)
    #change the file path once models are finalized and placed in the package directory
    beepr::beep(sound=5)
    message("***model upload complete***")
  })
  observeEvent(input$run2,{
    message("running segmentation model - please wait")
    resize_img<-resize(read_images[[1]], w = 256,h = 256)
    new_array<-resize_img%>%
      array_reshape(.,c(1,dim(.)))
    mask<-model%>%predict(new_array)%>%
      get_masks(binary_colormap)
    mask_main<<-as.array(mask)
    for (z in 2:length(read_images)){
      resize_img<-resize(read_images[[z]], w = 256,h = 256)
      new_array<-resize_img%>%image_to_array()%>%
        array_reshape(.,c(1,dim(.)))%>%
        '/'(255)
      mask<-model%>%predict(new_array)%>%
        get_masks(binary_colormap)
      mask_main <<- append(mask_main, mask)
    }
    beepr::beep(sound=5)
    message("***segmentation prediction complete***")
  })

  ####server - tab 3####
  observeEvent(input$run3,{
    message("running ID prediction model - please wait")
    beepr::beep(sound=5)
    message("cyanobacteria ID prediction complete")
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

runGadget(ui_main, server_main, viewer = dialogViewer("CyanoSCOPE - Prediction and Enumeration UI",width = 1600, height = 800))
