#' Point Select
#'
#' text goes here.
#'

point_select<-function(){
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  seed.input <<- data.frame(x_axis = numeric(0), y_axis = numeric(0))

  create_image <- function(loaded_image, image_data) {

    displayed_image <- loaded_image +
      geom_point(data = image_data, aes(x = .data$x_values,
                                        y = .data$y_values)) +
      geom_path(data = image_data, aes(x = .data$x_values,
                                       y = .data$y_values
      ),
      color = "black") +
      geom_point(data=seed.input,aes(x=.data$x_axis,
                                     y=.data$y_axis),
                 color="red")
    return(displayed_image)
  }

  ui1 <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    sidebarLayout(
      sidebarPanel(
        actionButton("close", "Close window",class = "btn-danger",style='height:75px;width:175px;font-size:140%',icon=icon("check"),style="display:center-align"),
        actionButton("BRefresh","Refresh",class = "btn-success",style='height:75px;width:175px;font-size:140%',icon=icon("arrows-rotate"),style="display:center-align"),
        h3(" "),
        DT::dataTableOutput('data')
      ),
      mainPanel(
        plotOutput("current_image_plot", dblclick = "double_click", hover = "hover", width = "100%"),
        h3(" "),
        h3(" ")
      )
    )
  )

  server1 <- function(input, output, session) {

    image_data <- shiny::reactiveValues()
    image_data$double_click <- data.frame(x_values=c(NA_real_,NA_real_), y_values = c(NA_real_,NA_real_))

    loaded_image <- magick::image_ggplot(image_modulate(image_read(images[[y]]),brightness=1000))

    output$current_image_plot <- renderPlot({
      displayed_image <- create_image(loaded_image,
                                      image_data$double_click
      )
      return(displayed_image)
    }, height=800,width=1000)

    rv <- reactiveVal(seed.input)

    output$data <- DT::renderDataTable ({
      DT::datatable(rv(), editable = TRUE)
    })

    observeEvent({input$double_click}, {
      clickrow <<- data.frame(x_values = input$double_click$x,
                              y_values = input$double_click$y)

      seed.input[nrow(seed.input) + 1, ] <<- c(clickrow$x_values,clickrow$y_values)

      image_data$double_click[1,] <<- clickrow

    })

    onclick("BRefresh",{
      proxy=dataTableProxy("data")
      replaceData(proxy,seed.input)
    })

    observeEvent(input$close, {
      js$closeWindow()
      shinyjs::stopApp()
    })

    session$onSessionEnded(function() {
      stopApp()
    })

  }

  runGadget(ui1, server1, viewer = dialogViewer("cellcount Image Analysis Interface",
                                                width = 1500, height = 2000))
}
