library(EBImage)
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
library(DT)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

image_num1<<-textConnection('image_num2','wr',local=FALSE)

ui2 <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  sidebarLayout(
    sidebarPanel(
      actionButton("previous", "Previous", icon=icon("arrow-left")),
      actionButton("next", "Next", icon=icon("arrow-right")),
      h6(" "),
      h6(" "),
      textOutput("image_num"),
      h6(" "),
      h6(" "),
      actionButton("run1", "Select Image Removal",class = "btn-success",icon=icon("trash")),
      h6(" "),
      h6(" "),
      actionButton("close", "Close window",class = "btn-danger",icon=icon("check"))
    ),
    mainPanel(
      plotOutput("current_image_plot")
    )
  )
)

server2 <- function(input, output, session) {
  index <- reactiveVal(1)

  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })

  observeEvent(input[["next"]], {
    index(min(index()+1, dim(st_img_test)[3]))
  })

  output$current_image_plot <- renderPlot({
    loaded_image <- magick::image_ggplot(image_read(st_img_test[,,index()]))
    loaded_image
  },res=300,width=350,height=350)

  output$image_num<-renderText({
    paste('Image number: ',index())
  })

  observeEvent(input$run1, {
    shinyalert::shinyalert('Enter image numbers for removal',
                           type='input',callbackR=image_number, showCancelButton = TRUE,
                           inputPlaceholder = 'Example: 1, 2, 3, 4, 5',
                           size="m")
  })

  image_number<-function(value4) {
    sink(image_num1)
    save_name<-cat(value4)
    sink()
    close(image_num1)
    image_num2<<-scan(text=image_num2,dec=",")
  }

  observeEvent(input$close, {
    js$closeWindow()
    shinyjs::stopApp()
  })

}

runGadget(ui2, server2, viewer = dialogViewer("Cell Image Selector",
                                            width = 800, height = 400))
