#' Point Select
#'
#' text goes here.
#'

point_select <- function(){
  jscode <- "shinyjs.closeWindow = function() { window.close(); }"
  js <- '$(document).ready(function(){
          var element = document.getElementById("current_image_plot");
          var instance = panzoom(element);
          $("#zoomout").on("dblclick", function(){
            return false;
          });
          $("#zoomin").on("dblclick", function(){
            return false;
          });
        });'
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
                 color="red",size=0.75)
    return(displayed_image)
  }



  myImgResource<-('./BloomSCOPE_Logo.png')

  ui1 <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),
    sidebarLayout(
      sidebarPanel(
        plotOutput("logo_img",width="50%",height="50%"),
        h6(" "),
        actionButton("close", "Close window",class = "btn-danger",style='height:50px;width:285px;font-size:140%',icon=icon("check"),style="display:center-align"),
        actionButton("BRefresh","Refresh",class = "btn-success",style='height:50px;width:285px;font-size:140%',icon=icon("arrows-rotate"),style="display:center-align"),
        h3(" "),
        DT::dataTableOutput('data'),
        tags$head(
          tags$script(src = "https://unpkg.com/panzoom@9.4.0/dist/panzoom.min.js"),
          tags$script(HTML(js))
        ),
        actionGroupButtons(
          inputIds = c("zoomout", "zoomin"),
          labels = list(icon("minus"), icon("plus")),
          status = "primary"
        ),
        width=3,style = "font-size:75%"),
      mainPanel(
        plotOutput("current_image_plot", click = "double_click", hover = "hover", width = "100%"),
        h3(" "),
        h3(" ")
      )
    )
  )

  server1 <- function(input, output, session) {

    image_data <- shiny::reactiveValues()
    image_data$double_click <- data.frame(x_values=c(NA_real_,NA_real_), y_values = c(NA_real_,NA_real_))

    loaded_image <- magick::image_ggplot(image_modulate(image_read(rgb.imgs)))
    loaded_logo <- magick::image_ggplot(image_modulate(image_read(myImgResource)))

    output$logo_img<-renderPlot({
      loaded_logo
      return(loaded_logo)
    }, height=100,width=285)


    output$current_image_plot <- renderPlot(execOnResize = FALSE,{
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

  runGadget(ui1, server1, viewer = dialogViewer("CyanoSCOPE Cell-Select Interface",
                                                width = 1400, height = 2000))
}
