# select location
selectlocationUI <- function(id){
  ns <- NS(id)
  tagList(
         HTML("<div class='subheader'><h2>REGION</h2></div>"),
         selectInput(
           inputId = ns("spatial_type"),
           label = NULL,
           choices = list(
             "ABS SA2 regions" = "abs_sa2",
             "Federal Electorates" = "electorates_federal"),
           width = "100%"),
         plotlyOutput(ns("plot_points"), height = "200px"),
         plotOutput(ns("map"), height = "200px"),
         # HTML("<div class='subheader'><h2>CLIMATE</h2></div>"),
         HTML("<br>"))
}

selectlocationServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      data <- reactiveValues(
        points = NULL,
        climate = NULL,
        polygons = NULL
        )
      outOfModule <- reactiveValues(
        selected_region = c(),
        AnnPrec = NULL,
        MaxTWarmMonth = NULL,
        MinTColdMonth = NULL,
        PrecSeasonality = NULL,
        latitude = NULL,
        AnnPrec = NULL,
        AnnTempRange = NULL,
        PrecSeasonality = NULL,
        PrecWarmQ = NULL
      )
      
      
      observeEvent(input$spatial_type, {
        if(input$spatial_type != "none"){
          data$points <- readRDS("data/sa2_points_climate.rds")
        }
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
            margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
            paper_bgcolor='transparent',
            plot_bgcolor = 'transparent'
          ) %>%
            config(displayModeBar = FALSE)
          # event_register(p, 'plotly_click')
        })
        
        # observe clicks on the region plot
        observe({
          if(!is.null(data$points)){
            click_region <- event_data(
              event = "plotly_click",
              source = "region_map"
            )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
            outOfModule$selected_region <- data$points$label[click_region]
            # add climate data
            climate_row <- which(data$points$label == outOfModule$selected_region)
            outOfModule$AnnPrec <- data$points$AnnPrec[climate_row]
            outOfModule$PrecSeasonality <- data$points$PrecSeasonality[climate_row]
          }
        })
        
        # draw a map
        output$map <- renderPlot({
          validate(need(outOfModule$selected_region, ""))
          data$polygons <- readRDS("data/sa2_polygons.rds")
          map_text <- data$points[data$points$label == outOfModule$selected_region, ]
          map_text$label <- paste(strsplit(map_text$label, " ")[[1]], collapse = "\n")
          ggplot(data$polygons[data$polygons$SA2_NAME16 == outOfModule$selected_region, ]) +
            geom_sf(fill = "grey90", color = "grey30") +
            geom_text(data = map_text,
                      mapping = aes(x = longitude, y = latitude, label = label),
                      color = "grey30",
                      alpha = 0.5,
                      size = 5
            ) +
            theme_void()
        })
      })
      return(outOfModule)
    }
  )
}