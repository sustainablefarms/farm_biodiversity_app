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
         fluidRow(
           column(width = 8,
             # if (isTRUE(getOption("shiny.testmode"))){
               # actionButton(ns("fake_region_number"), label = "Next Region")
             # } else {
               plotly::plotlyOutput(ns("plot_points"), height = "350px")
             # }
             ),
           column(width = 4, 
             # style = "
             # position: absolute;
             # top: 50%;
             # -ms-transform: translateY(-50%);
             # transform: translateY(-50%);",
             tags$div(style = "align: center;", textOutput(ns("regionname"), inline = TRUE)),
             plotOutput(ns("map"), height = "200px")
             )
         ),
         HTML("<div class='subheader'><h2>LONG-TERM AVERAGE</h2></div>"),
         fluidRow(
           column(width = 6,
              uiOutput(ns("show_maxtemp")),
              uiOutput(ns("show_mintemp"))
            ),
            column(width = 6,
              uiOutput(ns("show_precip_warm")),
              uiOutput(ns("show_precip_cold"))
            )
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
        polygons = NULL,
        points = NULL
        )
      outOfModule <- reactiveValues(
        locationcomplete = FALSE,
        selected_region = c(),
        MaxTWarmMonth.lt = NULL,
        PrecWarmQ.lt = NULL,
        MinTColdMonth.lt = NULL,
        PrecColdQ.lt = NULL,
        AnnPrec.lt = NULL,
        PrecSeasonality.lt = NULL,
        AnnMeanTemp.YfA     = NULL,
        MaxTWarmMonth.YfA   = NULL,
        PrecWarmQ.YfA       = NULL,
        MinTColdMonth.YfA   = NULL,
        PrecColdQ.YfA       = NULL,
        PrecSeasonality.YfA = NULL
      )
      click_values <- reactiveValues(
        climate = NULL,
        climate_title = NULL)

      # observeEvent(input$spatial_type, {
        # if(input$spatial_type != "none"){
          data$points <- readRDS("data/sa2_points_climate.rds")
        # }

        # draw a scatterplot of the centroids of selected zones
        output$plot_points <- plotly::renderPlotly({
          validate(
            need(data$points, "")
          )
          plotly::plot_ly(
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
          ) %>%
            add_sf(data = readRDS("./data/state_borders.rds"), 
                   type = "scatter", 
                   mode = "lines",
                   inherit = FALSE,
                   showlegend = FALSE,
                   hoverinfo = 'none',
                   line = list(color = "gray")
          ) %>% 
  add_text(x = min(data$points$longitude)+1, 
           y = c(max(data$points$latitude)-1, min(data$points$latitude) - 0.5),
           text = c("NSW", "VIC"), 
           textfont = list(size = 20),
           showlegend = FALSE,
           inherit = FALSE
           ) %>%
            plotly::layout(
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
          if(!is.null(data$points)){
            click_region <- plotly::event_data(
              event = "plotly_click",
              source = "region_map"
            )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
            outOfModule$selected_region <- data$points$label[click_region]
            # add climate data
            climate_row <- which(data$points$label == outOfModule$selected_region)
            outOfModule$MaxTWarmMonth.lt <- data$points$MaxTWarmMonth[climate_row]
            outOfModule$PrecWarmQ.lt <- data$points$PrecWarmQ[climate_row]
            outOfModule$MinTColdMonth.lt <- data$points$MinTColdMonth[climate_row]
            outOfModule$PrecColdQ.lt <- data$points$PrecColdQ[climate_row]
            outOfModule$PrecSeasonality.lt <- data$points$PrecSeasonality[climate_row]
            
            outOfModule$AnnPrec.lt <- data$points$AnnPrec[climate_row]
            outOfModule$AnnMeanTemp.YfA <- data$points$AnnMeanTemp[climate_row]/10
            outOfModule$MaxTWarmMonth.YfA <- new_data_mean$MaxTWarmMonth.YfA
            outOfModule$PrecWarmQ.YfA <- new_data_mean$PrecWarmQ.YfA
            outOfModule$MinTColdMonth.YfA <- new_data_mean$MinTColdMonth.YfA
	    outOfModule$PrecColdQ.YfA <- new_data_mean$PrecColdQ.YfA
            outOfModule$PrecSeasonality.YfA <- new_data_mean$PrecSeasonality.YfA
            if (length(outOfModule$selected_region) > 0){
              outOfModule$locationcomplete <- TRUE
            } else {
              outOfModule$locationcomplete <- FALSE
            }
          }
        })
        
        # insert region name
        output$regionname <- renderText({
          validate(need(outOfModule$selected_region, ""))
          outOfModule$selected_region
        })
        # draw a map
        output$map <- renderPlot({
          validate(need(outOfModule$selected_region, "Please select region"))
          data$polygons <- readRDS("data/sa2_polygons.rds")
          # map_text <- data$points[data$points$label == outOfModule$selected_region, ]
          # map_text$label <- paste(strsplit(map_text$label, " ")[[1]], collapse = "\n")
          ggplot(data$polygons[data$polygons$SA2_NAME16 == outOfModule$selected_region, ]) +
            geom_sf(fill = "grey90", color = "grey10") +
            # geom_text(data = map_text,
            #           mapping = aes(x = longitude, y = latitude, label = label),
            #           color = "grey30",
            #           alpha = 0.5,
            #           size = 5
            # ) +
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
          "Annual<br>Maximum<br>Temperature<h3>",
          round(outOfModule$MaxTWarmMonth.lt * 0.1, 1),
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
          "Annual<br>Minimum<br>Temperature<h3>",
          format(outOfModule$MinTColdMonth.lt * 0.1, digits = 3, trim = TRUE),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precip_warm <- renderUI({
    if(length(outOfModule$selected_region) > 0){
      actionButton2(
        inputId = ns("show_precip_warm_modal"),
        label = HTML(paste0(
          "Summer<br>Preciptiation<h3>",
          outOfModule$PrecWarmQ.lt,
          "mm</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precip_cold <- renderUI({
    if(length(outOfModule$selected_region) > 0){
      actionButton2(
        inputId = ns("show_precip_cold_modal"),
        label = HTML(paste0(
          "Winter<br>Preciptiation<h3>",
          outOfModule$PrecColdQ.lt,
          "mm</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$climate_plot <- renderPlot({
    if(!is.null(click_values$climate)){
      climate_plot(
        data = data$points,
        variable = click_values$climate,
        region = outOfModule$selected_region,
        title = click_values$climate_title)
    }
  })
  # run a different modal for each climate variable
  observeEvent(input$show_maxtemp_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "MaxTWarmMonth"
    click_values$climate_title <- "Annual Maximum temperature (Celsius)"
    climate_modal(ns, 
		  "The average annual maximum temperature from 1960 - 1990 was estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
    )
  })
  observeEvent(input$show_mintemp_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "MinTColdMonth"
    click_values$climate_title <- "Average Minimum Temperature (Celsius)"
    climate_modal(ns,
		  "The average annual minimum temperature from 1960 - 1990 was estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
  observeEvent(input$show_precip_warm_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "PrecWarmQ"
    click_values$climate_title <- "Summer Precipitation (mm)"
    climate_modal(ns,
		  "The summer precipitation is the average precipitation of the warmest quarter from 1960 - 1990 estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
  observeEvent(input$show_precip_cold_modal, {
    validate(need(outOfModule$selected_region, ""))
    click_values$climate <- "PrecColdQ"
    click_values$climate_title <- "Winter Precipitation (mm)"
    climate_modal(ns,
		  "The winter precipitation is the average precipitation of the coldest quarter from 1960 - 1990 estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
      
      outOfModule
    }
  )
}
