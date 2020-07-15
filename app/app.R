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
  column(width = 1),
  column(width = 3,
    selectInput(
      inputId = "spatial_type",
      label = "Select Zonation",
      choices = list(
        "None selected" = "none",
        "ABS SA2 regions" = "abs_sa2",
        "Federal Electorates" = "electorates_federal"
      )
    ),
    plotlyOutput("plot_points"),
    plotOutput("map")
  ),
  column(width = 4),
  fluidRow(
    column(width = 1),
    column(width = 4)
  ),
  title = "SF Model Visualiser",
  theme = shinytheme("lumen")
)

# SERVER
server <- function(input, output) {

  data <- reactiveValues(
    points = NULL,
    polygons = NULL,
    # regions = NULL,
    selected_region = NULL
  )

  # choose what spatial data to use
  observeEvent(input$spatial_type, {
    if(input$spatial_type != "none"){
      data$points <- readRDS("data/sa2_points.rds")
    }
  })

  # draw a scatterplot of the centroids of selected zones
  output$plot_points <- renderPlotly({
    validate(
      need(data$points, "")
    )
    plot_ly(
      data$points,
      x = ~longitude,
      y = ~latitude,
      type = "scatter",
      mode = "markers",
      source = "region_map",
      marker = list(
        size = 10,
        color = ~color
      ),
      hoverinfo = "text",
      text = ~label
    ) %>% layout(
      xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
      yaxis = list(scaleanchor = "x",
        title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
      margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
    )
    # event_register(p, 'plotly_click')
  })

  # observe clicks on the region plot
  observe({
    if(!is.null(data$points)){
      click_region <- event_data(
        event = "plotly_click",
        source = "region_map"
      )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
      data$selected_region <- data$points$label[click_region]
    }
  })

  # draw a map
  output$map <- renderPlot({
    validate(
      need(data$selected_region, "")
    )
    data$polygons <- readRDS("data/sa2_polygons.rds")
    map_text <- data$points[data$points$label == data$selected_region, ]
    map_text$label <- paste(strsplit(map_text$label, " ")[[1]], collapse = "\n")
    ggplot(data$polygons[data$polygons$SA2_NAME16 == data$selected_region, ]) +
      geom_sf() +
      geom_text(data = map_text,
        mapping = aes(x = longitude, y = latitude, label = label),
        color = "grey30",
        alpha = 0.5,
        size = 10
      ) +
      theme_void()
  })

} # end server

shinyApp(ui, server)