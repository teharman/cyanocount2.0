library(shiny)
library(magick)
library(ggplot2)
library(shinyalert)
library(shinyFiles)
library(shinyjs)
library(DT)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"
seed.input <<- data.frame(x_axis = character(0), y_axis = numeric(0))

create_image <- function(loaded_image, image_data) {

  displayed_image <- loaded_image +
    geom_point(data = image_data, aes(x = .data$x_values,
                                      y = .data$y_values)) +
    geom_path(data = image_data, aes(x = .data$x_values,
                                     y = .data$y_values
    ),
    color = "black")
  return(displayed_image)
}



ui <- fixedPage(
  useShinyjs(),
    mainPanel(
      plotOutput("current_image_plot", dblclick = "double_click", hover = "hover"),
      h3(" "),
      actionButton("close", "Close window",class = "btn-danger",style='height:75px;width:290px;font-size:140%',icon=icon("check"),style="display:center-align"),
      actionButton("BRefresh","Refresh",class = "btn-success",style='height:75px;width:300px;font-size:140%',icon=icon("arrows-rotate"),style="display:center-align"),
      h3(" "),
      h3(" "),
      DT::dataTableOutput('data')
    )
  )


server <- function(input, output, session) {

  image_data <- shiny::reactiveValues()
  image_data$double_click <- data.frame(x_values=c(NA_real_,NA_real_), y_values = c(NA_real_,NA_real_))

  loaded_image <- magick::image_ggplot(image_read(images[[1]]))

  output$current_image_plot <- renderPlot({
    displayed_image <- create_image(loaded_image,
                                    image_data$double_click
    )
    return(displayed_image)
  })

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

}

runGadget(ui, server, viewer = dialogViewer("cellcount Image Analysis Interface",
                                            width = 1000, height = 2300))
