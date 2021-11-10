# select location
selectlocationUI <- function(id){
  climate.lt <- readRDS("data/sa2_points_climate.rds")
  choices = list("Choose region" = "",
                 Victoria = climate.lt[climate.lt$state == "Victoria", "label"],
                 `New South Wales` = climate.lt[climate.lt$state == "New South Wales", "label"])
  
  ns <- NS(id)
  tagList(
	 waiter::use_waiter(),
	 twocolumns(
	   heading = "Select your region",
	   left = tagList(tags$p("We use your region to estimate your farm's climate."),
	                  infotext("Click map or select from list")),
	   right = 
	     tagList(
	       selectInput(ns("selectbox"), label = NULL, 
	                   choices = choices,
	                   multiple = FALSE,
	                   width = "100%"),
	       fluidRow(
	       column(width = 6, leaflet::leafletOutput(ns("regionsleaflet"))),
	       column(width = 6, tags$div(style = "align: center;", textOutput(ns("regionname"), inline = TRUE)),
	              plotOutput(ns("map"), width = "100%")))
	     )
	 ),
	 tags$div(id = ns("hide_until_region"),
	   # climate values
	   twocolumns(
	     heading = "View climate data",
	     left = "Shown here are historical climate averages for the centre of your region.",
	     right = tagList(
	 tags$div(class="row row-cols-1 row-cols-md-4 g-4",
  	 tags$div(class = "card",
  	   tags$div(class = "card-body",
  	            HTML("Annual Minimum<br>Temperature"),
  	            uiOutput(ns("mintemp"))
  	            )),
  	 tags$div(class = "card",
  	          tags$div(class = "card-body",
  	                   HTML("Annual Maximum<br>Temperature"),
  	                   uiOutput(ns("maxtemp"))
  	          )),
  	 tags$div(class = "card",
  	          tags$div(class = "card-body",
  	                   HTML("Summer<br>Precipitation"),
  	                   textOutput(ns("precip_warm"))
  	          )),
  	 tags$div(class = "card",
  	          tags$div(class = "card-body",
  	                   HTML("Winter<br>Precipitation"),
  	                   textOutput(ns("precip_cold"))
  	          ))
  	 ),
	 tags$div(class = "datalabels",
	          "Climate data estimated by",
	          tags$a(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
	     )),
	 twocolumns(heading = "Last year of rainfall",
	            left = tagList(tags$p(class = "bodysmall",
	                                  "A good guess is your region's average rainfall."),
	                           infotext("Drag to adjust rainfall amount")),
	            right = tagList(
	              sliderInput(
	                inputId = ns("AnnPrec.YfA"),
	                label = NULL,
	                min = 400, max = 1000, step = 20,
	                width = "100%",
	                value = new_data_mean$AnnPrec.YfA),
	              tags$div(textOutput(ns("annprec.lt.region"), inline = TRUE))
	            ))
         ))
}

selectlocationServer <- function(id, selected_region_outer, AnnPrec.YfA_outer, savebutton, cancelbutton){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      selected_region <- reactiveVal()
      data <- reactiveValues(
        climate = NULL,
        polygons = NULL,
        points = NULL
        )
      click_values <- reactiveValues(
        climate = NULL,
        climate_title = NULL)
      ns <- session$ns
      climate.lt <- readRDS("data/sa2_points_climate.rds")
      
      # sync selected_region_outer with selected region here, its ok to sensitive to both, because they can only change at the same time
      # from outer to internal for region (the YfA)
      observeEvent(selected_region_outer(), {
        if (selected_region() != selected_region_outer()){
          selected_region(selected_region_outer())}
      })  
      # from outer to out of module
      outofmodule <- reactive({
        ltclim <- ltcliminfo_region(selected_region_outer(), climdatatbl = data$points)
        yfainfo <- list(AnnPrec.YfA = AnnPrec.YfA_outer())
        print("updating from outer")
        c(ltclim, yfainfo) #output once outer is update
      })

      # from internal to outer
      observeEvent(savebutton(), {
        selected_region_outer(selected_region())
        AnnPrec.YfA_outer(input$AnnPrec.YfA)
      })
      observeEvent(cancelbutton(), {
        selected_region(selected_region_outer())
      })
      

      # observeEvent(input$spatial_type, {
        # if(input$spatial_type != "none"){
          data$points <- readRDS("data/sa2_points_climate.rds")
        # }
        wleaflet <- waiter::Waiter$new(id = ns("regionsleaflet"))

        
      # leaflet map operations
      output$regionsleaflet <- leaflet::renderLeaflet({
        wleaflet$show()
	      on.exit(wleaflet$hide())
        regionplot_leaflet()
      })
      # update selected_region based on leaflet click
      observe({
        p <- input$regionsleaflet_shape_click
        validate(need(p, ""))
        containing_region <- lonlat2region(p$lng,p$lat)
        if (length(containing_region) > 1){
          showNotification("Please click in the interior of the region")
        } else {
          selected_region(containing_region)
        }
      })
      #update map to show selected region if selected another way
      observeEvent(selected_region(), {
          lftproxy <- leaflet::leafletProxy("regionsleaflet") %>%
            leaflet::removeShape("selectedpolygon")
            if (isTruthy(selected_region())){
              regionpolygon <- regionpolygons_4326[regionpolygons_4326$SA2_NAME16 == selected_region(), ]
              regionpt <- regionpts[regionpts$label == selected_region(), ]
              lftproxy %>% 
              leaflet::addPolygons(data = regionpolygon,
                                   color = "red", layerId = "selectedpolygon") %>%
              leaflet::addPopups(lng = regionpt$longitude, 
                                   lat = regionpt$latitude,
                                   popup = regionpt$label,
                                 options = leaflet::popupOptions(closeOnClick = FALSE,
                                                        closeOnEscapeKey = FALSE,
                                                        autoclose = FALSE,
                                                        closeButton = FALSE),
                                 layerId = "selectedpolygon")
            }
      }, priority = 101)
        
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
       ltcliminfo <- reactive({
          locinfo <- ltcliminfo_region(selected_region(), climdatatbl = data$points)
          locinfo
        }) %>% throttle(1000)
        
        # insert region name
        output$regionname <- renderText({
          validate(need(selected_region(), ""))
          selected_region()
        })
        # draw a map
        output$map <- renderPlot({
          validate(need(selected_region(), "Please select your region"))
          # map_text <- data$points[data$points$label == outOfModule()$selected_region, ]
          # map_text$label <- paste(strsplit(map_text$label, " ")[[1]], collapse = "\n")
          ggplot(regionpolygons_4326[regionpolygons_4326$SA2_NAME16 == selected_region(), ]) +
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
      
  ## CLIMATE values render output
  output$maxtemp <- renderUI({
    validate(need(ltcliminfo()$MaxTWarmMonth.lt, ""))
    HTML(paste0(round(ltcliminfo()$MaxTWarmMonth.lt * 0.1, 1),
                "&deg;C"))
  })
  
  output$mintemp <- renderUI({
    validate(need(ltcliminfo()$MinTColdMonth.lt, ""))
    HTML(paste0(round(ltcliminfo()$MinTColdMonth.lt * 0.1, 1),
                "&deg;C"))
  })

  output$precip_warm <- renderText({
    validate(need(ltcliminfo()$PrecWarmQ.lt, ""))
    HTML(paste0(round(ltcliminfo()$PrecWarmQ.lt * 0.1, 1),
                "mm"))
  })
  
  output$precip_cold <- renderText({
    validate(need(ltcliminfo()$PrecColdQ.lt, ""))
    HTML(paste0(round(ltcliminfo()$PrecColdQ.lt * 0.1, 1),
                "mm"))
  })
    
  ## YfA
  # update YfA based on new location info
  observeEvent(selected_region(), {
      	validate(need(selected_region(), ""))
        climate_row <- which(climate.lt$label == selected_region())
	updateSliderInput(inputId = "AnnPrec.YfA",
			  value = climate.lt$AnnPrec[climate_row])
      }, priority = 100, ignoreInit = TRUE, ignoreNULL = TRUE)
  # whenever both inputs change at the same time, do an update from AnnPrec.YfA_outer *second*
  observeEvent(AnnPrec.YfA_outer(), {
    validate(need(AnnPrec.YfA_outer(), ""))
    if (AnnPrec.YfA_outer() != input$AnnPrec.YfA){
      updateSliderInput(inputId = "AnnPrec.YfA",
                        value = AnnPrec.YfA_outer())
    }
  }, priority = -1, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  output$annprec.lt.region <- renderText({
    validate(need(selected_region(), ""))
    climate_row <- which(climate.lt$label == selected_region())
    sprintf("(long term average for %s: %imm)",
            selected_region(),
            climate.lt$AnnPrec[climate_row])
  })
  
  # hide a section
  observeEvent(selected_region(), {
   shinyjs::toggleElement(id = "hide_until_region",
                   condition = isTruthy(selected_region()))
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  if (isTRUE(getOption("shiny.testmode"))){
    observeEvent(selected_region(), {showNotification(paste("Selected region is", selected_region()))})
  }

  setBookmarkExclude(c("show_maxtemp_modal",
                       "plot_points_waiter_hidden",
                       "show_precip_warm_modal",
                       "show_mintemp_modal",
                       "show_precip_cold_modal"))
  
  
  observe({
    outofmodule()
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
      
  outofmodule
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
