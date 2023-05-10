library(shiny)
library(magick)
library(ggplot2)


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



ui <- fluidPage(

  titlePanel(""),

  sidebarLayout(
    sidebarPanel(
    ),

    mainPanel(
      plotOutput("current_image_plot", dblclick = "double_click", hover = "hover")
    )
  )
)

server <- function(input, output) {

  image_data <- shiny::reactiveValues()
  image_data$double_click <- data.frame(x_values=c(NA_real_,NA_real_), y_values = c(NA_real_,NA_real_))

  loaded_image <- magick::image_ggplot(image_read(images[[1]]))

  output$current_image_plot <- renderPlot({
    displayed_image <- create_image(loaded_image,
                                    image_data$double_click
    )
    return(displayed_image)
  })

  observeEvent({input$double_click}, {
    clickrow <<- data.frame(x_values = input$double_click$x,
                           y_values = input$double_click$y)

    image_data$double_click[1,] <<- clickrow

  })



}

shinyApp(ui = ui, server = server)
