# select location
selectlocationUI <- function(id){
  climate.lt <- readRDS("data/sa2_points_climate.rds")
  choices = list("Choose region" = "",
                 Victoria = climate.lt[climate.lt$state == "Victoria", "label"],
                 `New South Wales` = climate.lt[climate.lt$state == "New South Wales", "label"])
  
  ns <- NS(id)
  tagList(
	 waiter::use_waiter(),
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
             selectInput(ns("selectbox"), label = NULL, 
                         choices = choices,
                         multiple = FALSE),
             plotly::plotlyOutput(ns("plot_points"), height = "350px")
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

selectlocationServer <- function(id, selected_region){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      regionmapcreated <- reactiveVal(FALSE)
      data <- reactiveValues(
        climate = NULL,
        polygons = NULL,
        points = NULL
        )
      click_values <- reactiveValues(
        climate = NULL,
        climate_title = NULL)
      ns <- session$ns

      # observeEvent(input$spatial_type, {
        # if(input$spatial_type != "none"){
          data$points <- readRDS("data/sa2_points_climate.rds")
        # }
        wplotly <- waiter::Waiter$new(id = ns("plot_points"))

        # draw a scatterplot of the centroids of selected zones
        output$plot_points <- plotly::renderPlotly({
	  wplotly$show()
          validate(
            need(data$points, message = "no data points yet")
          )
	  on.exit(wplotly$hide())
	    out <- regionplot_borders(ns("region_map"))
          regionmapcreated(TRUE)
          return(out)
        })
        
        # observe clicks on the region plot
        if (FALSE & isTRUE(getOption("shiny.testmode"))){
          selected_region(data$points$label[input$fake_region_number])
        } else {
        observe({
          if(!is.null(data$points)){ # if data points supplied (currently they always are)
            validate(need(regionmapcreated(), ""))
            click_region <- plotly::event_data(
              event = "plotly_click",
              source = ns("region_map")
            )$key %>% unlist() #key is more reliable than pointNumber, and also works on the polygon version
            if (isTruthy(click_region)){
              selected_region(click_region)
            }
          }
        })
        }
        
       # create observers (reactive end points) for refactoring the inputs
       observeEvent(input$selectbox, {
         validate(need(input$selectbox, ""))
         selected_region(input$selectbox)
       })
       observeEvent(selected_region(),{
                validate(need(selected_region(), ""))
                updateSelectInput(inputId = "selectbox",
                                  selected = selected_region())
       }, ignoreInit = FALSE)
       
       # obtain actual climate if required by later work
        outOfModule <- reactive({
          locinfo <- list()
            locinfo$selected_region <- selected_region()
            # add climate data
            climate_row <- which(data$points$label == locinfo$selected_region)
            locinfo$MaxTWarmMonth.lt <- data$points$MaxTWarmMonth[climate_row]
            locinfo$PrecWarmQ.lt <- data$points$PrecWarmQ[climate_row]
            locinfo$MinTColdMonth.lt <- data$points$MinTColdMonth[climate_row]
            locinfo$PrecColdQ.lt <- data$points$PrecColdQ[climate_row]
            locinfo$PrecSeasonality.lt <- data$points$PrecSeasonality[climate_row]
            
            locinfo$AnnPrec.lt <- data$points$AnnPrec[climate_row]
            locinfo$AnnMeanTemp.YfA <- data$points$AnnMeanTemp[climate_row]/10
            locinfo$MaxTWarmMonth.YfA <- new_data_mean$MaxTWarmMonth.YfA
            locinfo$PrecWarmQ.YfA <- new_data_mean$PrecWarmQ.YfA
            locinfo$MinTColdMonth.YfA <- new_data_mean$MinTColdMonth.YfA
            locinfo$PrecColdQ.YfA <- new_data_mean$PrecColdQ.YfA
            locinfo$PrecSeasonality.YfA <- new_data_mean$PrecSeasonality.YfA
            if (isTruthy(locinfo$selected_region)){
              locinfo$locationcomplete <- TRUE
            } else {
              locinfo$locationcomplete <- FALSE
            }
            
            locinfo
        }) %>% throttle(1000)
        
        # insert region name
        output$regionname <- renderText({
          validate(need(outOfModule()$selected_region, ""))
          outOfModule()$selected_region
        })
        # draw a map
        output$map <- renderPlot({
          validate(need(outOfModule()$selected_region, "Please select your region"))
          data$polygons <- readRDS("data/sa2_polygons.rds")
          # map_text <- data$points[data$points$label == outOfModule()$selected_region, ]
          # map_text$label <- paste(strsplit(map_text$label, " ")[[1]], collapse = "\n")
          ggplot(data$polygons[data$polygons$SA2_NAME16 == outOfModule()$selected_region, ]) +
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

  output$show_maxtemp <- renderUI({
    if(isTruthy(outOfModule()$locationcomplete)){
      actionButton2(
        inputId = ns("show_maxtemp_modal"),
        label = HTML(paste0(
          "Annual<br>Maximum<br>Temperature<h3>",
          round(outOfModule()$MaxTWarmMonth.lt * 0.1, 1),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_mintemp <- renderUI({
    if(isTruthy(outOfModule()$locationcomplete)){
      actionButton2(
        inputId = ns("show_mintemp_modal"),
        label = HTML(paste0(
          "Annual<br>Minimum<br>Temperature<h3>",
          format(outOfModule()$MinTColdMonth.lt * 0.1, digits = 3, trim = TRUE),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precip_warm <- renderUI({
    if(isTruthy(outOfModule()$locationcomplete)){
      actionButton2(
        inputId = ns("show_precip_warm_modal"),
        label = HTML(paste0(
          "Summer<br>Precipitation<h3>",
          outOfModule()$PrecWarmQ.lt,
          "mm</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precip_cold <- renderUI({
    if(isTruthy(outOfModule()$locationcomplete)){
      actionButton2(
        inputId = ns("show_precip_cold_modal"),
        label = HTML(paste0(
          "Winter<br>Precipitation<h3>",
          outOfModule()$PrecColdQ.lt,
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
        region = outOfModule()$selected_region,
        title = click_values$climate_title)
    }
  })
  # run a different modal for each climate variable
  observeEvent(input$show_maxtemp_modal, {
    validate(need(outOfModule()$selected_region, ""))
    click_values$climate <- "MaxTWarmMonth"
    click_values$climate_title <- "Annual Maximum temperature (Celsius)"
    climate_modal(ns, 
		  "The average annual maximum temperature from 1960 - 1990 was estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
  observeEvent(input$show_mintemp_modal, {
    validate(need(outOfModule()$selected_region, ""))
    click_values$climate <- "MinTColdMonth"
    click_values$climate_title <- "Average Minimum Temperature (Celsius)"
    climate_modal(ns,
		  "The average annual minimum temperature from 1960 - 1990 was estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
  observeEvent(input$show_precip_warm_modal, {
    validate(need(outOfModule()$selected_region, ""))
    click_values$climate <- "PrecWarmQ"
    click_values$climate_title <- "Summer Precipitation (mm)"
    climate_modal(ns,
		  "The summer precipitation is the average precipitation of the warmest quarter from 1960 - 1990 estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
  observeEvent(input$show_precip_cold_modal, {
    validate(need(outOfModule()$selected_region, ""))
    click_values$climate <- "PrecColdQ"
    click_values$climate_title <- "Winter Precipitation (mm)"
    climate_modal(ns,
		  "The winter precipitation is the average precipitation of the coldest quarter from 1960 - 1990 estimated by",
                  linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
  })
  
  setBookmarkExclude(c("show_maxtemp_modal",
                       "plot_points_waiter_hidden",
                       "show_precip_warm_modal",
                       "show_mintemp_modal",
                       "show_precip_cold_modal"))
  
  
  observe({
    outOfModule()
    session$doBookmark()
  })
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    # if (length(selected_region()) == 0){
      # state$values$selected_region <- "None"
    # } else {
      state$values$selected_region <- selected_region()
    # }
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    # url converts "" values to list() values so below needed to fix it
    urlselected_region <- state$values$selected_region
    if (length(urlselected_region) == 0){
      selected_region("")
    } else {
      selected_region(urlselected_region)
    }
  })
      
      outOfModule
    }
  )
}

app_selectlocation <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {bootstrapPage(
      includeCSS("./www/base.css"),
      fluidRow(selectlocationUI("location")),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region <- reactiveVal("Nagambie")
      refresh <- reactiveTimer(1000 * 30)
      observeEvent(refresh(), {
        selected_region("Temora")
      })
      selectlocationServer("location", selected_region)
    })
}
