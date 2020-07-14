# shiny app
library(shiny)
library(shinythemes)


# UI
ui <- fluidPage(
  includeCSS("./www/base.css"),
  fluidRow(HTML("
    <div class='header'>
      <img class='logo' src='SF Logo Vertical - Transparent Background.png' alt='SF logo'>
      <span class='main'>BGGW Bird Biodiversity Estimator</span>
      <span class='subtitle'>by Martin Westgate & Kassel Hingee</span>
    </div>
  ")),
  fluidRow(
    column(width = 2),
    column(width = 4,
      selectInput(
        inputId = "spatial_type",
        label = "Select Zonation",
        choices = list(
          "None selected" = "none",
          "ABS SA2 regions" = "abs_sa2",
          "Federal Electorates" = "electorates_federal"
        )
      )
    ),
    column(width = 4,
      uiOutput(outputId = "region_selector")
    )
  ),
  fluidRow(
    column(width = 1),
    column(width = 4,
      plotOutput("map")
    )
  ),
  title = "SF Model Visualiser",
  theme = shinytheme("lumen")
)

# SERVER
server <- function(input, output) {

  data <- reactiveValues(
    spatial = NULL,
    regions = NULL,
    selected_region = NULL
  )

  # choose what spatial data to use
  observeEvent(input$spatial_type, {
    if(input$spatial_type != "none"){
      data$spatial <- readRDS("data/spatial_sa2.rds")
      data$regions <- data$spatial$SA2_NAME16
    }
  })

  # choose a region
  output$region_selector <- renderUI({
    if(!is.null(data$spatial)){
      selectInput(
        inputId = "region_choice",
        label = "Select region",
        choices = data$regions
      )
    }
  })

  # save region choice
  observeEvent(input$region_choice, {
    if(is.null(data$spatial)){
      data$selected_region <- NULL
    }else{
      data$selected_region <- input$region_choice
    }
  })

  # draw a map
  output$map <- renderPlot({
    if(!is.null(data$selected_region)){
      ggplot(data$spatial[data$spatial$SA2_NAME16 == data$selected_region, ]) +
        geom_sf() +
        theme_void()
    }
  })

} # end server

shinyApp(ui, server)