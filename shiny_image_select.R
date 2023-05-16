library(shiny)

ui_test <- fluidPage(

  titlePanel("Slideshow"),

  sidebarLayout(
    sidebarPanel(
      actionButton("previous", "Previous"),
      actionButton("next", "Next")
    ),

    mainPanel(
      imageOutput("image")
    )
  )
)

server_test <- function(input, output, session) {

  index <- reactiveVal(1)

  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })
  observeEvent(input[["next"]], {
    index(min(index()+1, length(st_blob_test)))
  })

  output$image <- renderDisplay({
    display(st_blob_test)
  })
}

# Run the application
shinyApp(ui = ui_test, server = server_test)
