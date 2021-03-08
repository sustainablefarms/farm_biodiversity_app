# select location
selectlocationUI <- function(id){
  ns <- NS(id)
  tagList(
         HTML("<div class='subheader'><h2>REGION</h2></div>"),
         # selectInput(
         #   inputId = ns("spatial_type"),
         #   label = NULL,
         #   choices = list(
         #     "ABS SA2 regions" = "abs_sa2",
         #     "Federal Electorates" = "electorates_federal"),
         #   width = "100%"),
         plotly::plotlyOutput(ns("plot_points"), height = "200px"),
         plotOutput(ns("map"), height = "200px"),
         # HTML("<div class='subheader'><h2>CLIMATE</h2></div>"),
         HTML("<br>"),
         column(width = 6,
              uiOutput(ns("show_maxtemp")),
              uiOutput(ns("show_mintemp"))
            ),
            column(width = 6,
              uiOutput(ns("show_precip")),
              uiOutput(ns("show_precseason"))
            )
         )
}

selectlocationServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      data <- reactiveValues(
        climate = NULL,
        polygons = NULL
        )
      outOfModule <- reactiveValues(
        points = NULL,
        selected_region = c(),
        AnnPrec = NULL,
        MaxTWarmMonth = NULL,
        MinTColdMonth = NULL,
        PrecSeasonality = NULL,
        latitude = NULL
      )
      click_values <- reactiveValues(
        climate = NULL,
        climate_title = NULL)

      
      # observeEvent(input$spatial_type, {
        # if(input$spatial_type != "none"){
          outOfModule$points <- readRDS("data/sa2_points_climate.rds")
        # }

        # draw a scatterplot of the centroids of selected zones
        output$plot_points <- plotly::renderPlotly({
          validate(
            need(outOfModule$points, "")
          )
          plotly::plot_ly(
            outOfModule$points,
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
          ) %>% plotly::layout(
            xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
            yaxis = list(scaleanchor = "x",
                         title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
            margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
            paper_bgcolor='transparent',
            plot_bgcolor = 'transparent'
          ) %>%
            plotly::config(displayModeBar = FALSE)
          # event_register(p, 'plotly_click')
        })
        
        # observe clicks on the region plot
        observe({
          if(!is.null(outOfModule$points)){
            click_region <- plotly::event_data(
              event = "plotly_click",
              source = "region_map"
            )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
            outOfModule$selected_region <- outOfModule$points$label[click_region]
            # add climate data
            climate_row <- which(outOfModule$points$label == outOfModule$selected_region)
            outOfModule$AnnPrec <- outOfModule$points$AnnPrec[climate_row]
            outOfModule$MaxTWarmMonth = outOfModule$points$MaxTWarmMonth[climate_row]
            outOfModule$MinTColdMonth = outOfModule$points$MinTColdMonth[climate_row]
            outOfModule$PrecSeasonality = outOfModule$points$PrecSeasonality[climate_row]
            outOfModule$latitude = outOfModule$points$latitude[climate_row]
          }
        })
        
        # draw a map
        output$map <- renderPlot({
          validate(need(outOfModule$selected_region, ""))
          data$polygons <- readRDS("data/sa2_polygons.rds")
          map_text <- outOfModule$points[outOfModule$points$label == outOfModule$selected_region, ]
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
      # })
      
  ## CLIMATE buttons and plots
      ns <- session$ns
  output$show_maxtemp <- renderUI({
    if(length(outOfModule$selected_region) > 0){
      actionButton2(
        inputId = ns("show_maxtemp_modal"),
        label = HTML(paste0(
          "Maximum<br>Temperature<h3>",
          round(outOfModule$MaxTWarmMonth * 0.1, 1),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_mintemp <- renderUI({
    if(length(outOfModule$selected_region) > 0){
      actionButton2(
        inputId = ns("show_mintemp_modal"),
        label = HTML(paste0(
          "Minimum<br>Temperature<h3>",
          format(outOfModule$MinTColdMonth * 0.1, digits = 3, trim = TRUE),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precip <- renderUI({
    if(length(outOfModule$selected_region) > 0){
      actionButton2(
        inputId = ns("show_precip_modal"),
        label = HTML(paste0(
          "Annual<br>Preciptiation<h3>",
          outOfModule$AnnPrec,
          "mm</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precseason <- renderUI({
    if(length(outOfModule$selected_region) > 0){
      actionButton2(
        inputId = ns("show_precseason_modal"),
        label = HTML(paste0(
          "Precipitation<br>Seasonality<h3>",
          outOfModule$PrecSeasonality,
          "</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$climate_plot <- renderPlot({
    if(!is.null(click_values$climate)){
      climate_plot(
        data = outOfModule$points,
        variable = click_values$climate,
        region = outOfModule$selected_region,
        title = click_values$climate_title)
    }
  })
  # run a different modal for each climate variable
  observeEvent(input$show_maxtemp_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "MaxTWarmMonth"
    click_values$climate_title <- "Maximum temperature (Celsius)"
    climate_modal(ns)
  })
  observeEvent(input$show_mintemp_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "MinTColdMonth"
    click_values$climate_title <- "Minimum temperature (Celsius)"
    climate_modal(ns)
  })
  observeEvent(input$show_precip_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "AnnPrec"
    click_values$climate_title <- "Annual precipitation (mm)"
    climate_modal(ns)
  })
  observeEvent(input$show_precseason_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "PrecSeasonality"
    click_values$climate_title <- "Precipitation seasonality"
    climate_modal(ns)
  })
      
      outOfModule
    }
  )
}
