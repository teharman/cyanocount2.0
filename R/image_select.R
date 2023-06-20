#' Image Select
#'
#' Shiny UI that helps users select and remove problematic segmented cells
#' for image library construction
#'

image_select<-function(){
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  image_number<<-data.frame(img_num=numeric(0))
  image_num1<<-textConnection('image_num2','wr',local=FALSE)
  myImgResource<-('C:/Users/Tyler.Harman/Desktop/cellcount_work/CyanoSCOPE_Logo.png')

  ui2 <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    sidebarLayout(
      sidebarPanel(
        plotOutput("logo_img",width="75%",height="75%"),
        h6(" "),
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
        actionButton("close", "Close window",class = "btn-danger",icon=icon("check")),
        width=4),
      mainPanel(
        plotOutput("current_image_plot")
      )
    )
  )

  server2 <- function(input, output, session) {
    index <- reactiveVal(1)

    loaded_logo <- magick::image_ggplot(image_modulate(image_read(myImgResource)))

    output$logo_img<-renderPlot({
      loaded_logo
      return(loaded_logo)
    }, height=100,width=200)

    observeEvent(input[["previous"]], {
      index(max(index()-1, 1))
    })

    observeEvent(input[["next"]], {
      index(min(index()+1, dim(st_img_test)[4]))
    })

    output$current_image_plot <- renderPlot({
      loaded_image <- magick::image_ggplot(image_read(st_img_test[,,,index()]))
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

    observeEvent(input[["run2"]], {
      image_number[nrow(image_number) + 1, ] <<- c(index())
    })

    image_number<-function(value4) {
      sink(image_num1,0)
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

  runGadget(ui2, server2, viewer = dialogViewer("CyanoSCOPE Cell Image Selector",
                                                width = 800, height = 400))
}
