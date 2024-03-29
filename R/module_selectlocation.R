# select location
selectlocationUI <- function(id){
  climate.lt <- readRDS("data/sa2_points_climate.rds")
  choices = list("Choose region" = "",
                 Victoria = climate.lt[climate.lt$state == "Victoria", "label"],
                 `New South Wales` = climate.lt[climate.lt$state == "New South Wales", "label"])
  
  ns <- NS(id)
  internals <-  tagList(
	 twocolumns(
	   heading = "Select your region",
	   left = tagList(tags$p(appname, "uses your region to estimate your farm's climate."),
	                  infotext("Click map or select from list"),
	                  uiOutput(ns("warn"))),
	   right = 
	     tagList(
	       selectInput(ns("selectbox"), label = NULL, 
	                   choices = choices,
	                   multiple = FALSE,
	                   width = "100%"),
	       fluidRow(
	       column(width = 6, 
		      tags$div(class = "position-relative",
			tags$div(class = "position-absolute h-100 w-100",
				 tags$div(class="position-absolute top-50 start-50 spinner-border"),
				 style = "z-index: 1001;",
				 style = "opacity: 80%;"),
		        leaflet::leafletOutput(ns("regionsleaflet")))
		      ),
	       column(width = 6, tags$div(style = "align: center;", textOutput(ns("regionname"), inline = TRUE)),
	              plotOutput(ns("map"), width = "100%")))
	     )
	 ),
	 tags$div(id = ns("hide_until_region"),
	   # climate values
	   twocolumns(
	     heading = "Climate averages",
	     left = "Historical temperature and rainfall averages for the centre of your region.",
	     right = tagList(
	 tags$div(class="row row-cols-1 row-cols-md-4 g-4",
	   mapply(function(name, uiid){
	     column(3, 
	     tags$div(class = "card h-100",
	              style = paste("background-color:", appcolors[["Green 10"]], ";"),
	              style = "border-radius: 0;",
	              style = "border-bottom-style: none; border-left-style:none; border-right-style: none;",
	              style = paste("border-color: ", appcolors[["Dark Green"]], ";"),
	              tags$div(class = "card-body d-flex justify-content-center", tags$div(
	                       tags$div(class = "bodysmall",
			       style = paste("color:", appcolors[["Dark Green"]], ";"),
	                       style = "text-align: center;",
	                       HTML(name)),
			       uiOutput(ns(uiid), style = "text-align: center;")
	              )))
	     )
	   },
	   list("Average winter minimum temperature",
	        "Average summer maximum Temperature",
	        "Average summer precipitation",
	        "Average winter precipitation"),
	   list("mintemp",
	        "maxtemp",
	        "precip_warm",
	        "precip_cold"
	        ),
	   SIMPLIFY = FALSE
	   )
  	 ),
	 tags$div(class = "datalabels",
	          "Climate data estimated by",
	          linknewtab(href = "https://www.worldclim.org/data/v1.4/worldclim14.html", "worldclim.org"))
	     )),
	 twocolumns(heading = "Recent rainfall",
	            left = tagList(tags$p(class = "bodysmall",
	                                  "Select the rainfall in the last year. The default is your region's historical yearly average."),
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
  
accitem <- accordion_item("Your region", id = ns("acc"), 
                          internals,
                          footer = tagList(
                            do.call(actionButton_notdfl,
                                    args = c(list(ns(paste0("cancel")), "Cancel", class = "btn-outline-primary",
		                                  onclick = paste0("scrollupby('", ns("acc"), "_body","')")),
                                             toggle_attr(paste0(ns("acc"), "_body"))
                                    )),
                            do.call(actionButton_notdfl,
                                    args = c(list(ns(paste0("save")), "Save and Close", class = "btn-primary",
		                                  onclick = paste0("scrollupby('", ns("acc"), "_body","')")),
                                             toggle_attr(paste0(ns("acc"), "_body"))
                                    ))
                          ),
                          footerdflt = "none"
)
  return(accitem)
}



selectlocationServer <- function(id, selected_region_outer, AnnPrec.YfA_outer){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      selected_region <- reactiveVal()
      showwarnings <- reactiveVal(FALSE)
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
        if (!isTruthy(selected_region() == selected_region_outer())){
          selected_region(selected_region_outer())}
      }) 
      # from outer to internal for YfA
      observeEvent(selected_region(), {
          	validate(need(selected_region(), ""))
            climate_row <- which(climate.lt$label == selected_region())
    	updateSliderInput(inputId = "AnnPrec.YfA",
    			  value = climate.lt$AnnPrec[climate_row])
          }, priority = 100, ignoreInit = TRUE, ignoreNULL = TRUE)
      # whenever both inputs change at the same time, do an update from AnnPrec.YfA_outer *second*
      observeEvent(AnnPrec.YfA_outer(), {
        validate(need(AnnPrec.YfA_outer(), ""))
        if (!isTruthy(AnnPrec.YfA_outer() == input$AnnPrec.YfA)){
          updateSliderInput(inputId = "AnnPrec.YfA",
                            value = AnnPrec.YfA_outer())
        }
      }, priority = -1, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      # from internal to outer
      observeEvent(input$save, {
        validate(need(selected_region(), ""))
        selected_region_outer(selected_region())
        AnnPrec.YfA_outer(input$AnnPrec.YfA)
        showwarnings(TRUE)
      })
      observeEvent(input$cancel, {
        selected_region(selected_region_outer())
        updateSliderInput(inputId = "AnnPrec.YfA",
                          value = AnnPrec.YfA_outer())
      })
      
      # from outer to out of module
      outofmodule <- reactive({
        ltclim <- ltcliminfo_region(selected_region_outer(), climdatatbl = climate.lt)
        yfainfo <- list(AnnPrec.YfA = AnnPrec.YfA_outer())
        c(ltclim, yfainfo) #output once outer is updated
      })

      # disable if nothing selected
      observe({shinyjs::toggleState(id = "save", isTruthy(selected_region()))})


        
      # leaflet map operations
      output$regionsleaflet <- leaflet::renderLeaflet({
        regionplot_leaflet()
      })

      # update selected_region based on leaflet click
      observe({
        p <- input$regionsleaflet_shape_click
        validate(need(p, ""))
        containing_region <- lonlat2region(p$lng,p$lat)
        if (length(containing_region) > 1){
          showNotification("Please click further into the interior of the region")
        } else {
          selected_region(containing_region)
        }
      })
      #update map to show selected region if selected another way
      observeEvent(selected_region(), {
          lftproxy <- leaflet::leafletProxy("regionsleaflet") %>%
            leaflet::removeShape("selectedpolygon") %>%
            leaflet::removePopup("selectedpolygon")
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
                validate(need(isTruthy(is.character(selected_region())), ""))
                updateSelectInput(inputId = "selectbox",
                                  selected = selected_region())
       }, ignoreInit = FALSE)
       
        # render region name
        output$regionname <- renderText({
          validate(need(selected_region(), ""))
          selected_region()
        })
        # draw a map
        output$map <- renderPlot({
          validate(need(selected_region(), "Please select your region"))
          ggplot(regionpolygons_4326[regionpolygons_4326$SA2_NAME16 == selected_region(), ]) +
            geom_sf(fill = "grey90", color = "grey10") +
            theme_void()
        })
      # })
      
  ## CLIMATE values render output
  ltclim_fordisplay <- reactive({ltcliminfo_region(selected_region(), climdatatbl = climate.lt)})
  output$maxtemp <- renderUI({
    validate(need(ltclim_fordisplay()$MaxTWarmMonth.lt, ""))
    tags$b(class = "bodylarge",
           style = paste("color:", appcolors[["Dark Green"]], ";"),
    HTML(paste0(round(ltclim_fordisplay()$MaxTWarmMonth.lt * 0.1, 1),
                "&deg;C")))
  })
  
  output$mintemp <- renderUI({
    validate(need(ltclim_fordisplay()$MinTColdMonth.lt, ""))
    tags$b(class = "bodylarge",
           style = paste("color:", appcolors[["Dark Green"]], ";"),
    HTML(paste0(round(ltclim_fordisplay()$MinTColdMonth.lt * 0.1, 1),
                "&deg;C")))
  })

  output$precip_warm <- renderUI({
    validate(need(ltclim_fordisplay()$PrecWarmQ.lt, ""))
    tags$b(class = "bodylarge",
           style = paste("color:", appcolors[["Dark Green"]], ";"),
    HTML(paste0(round(ltclim_fordisplay()$PrecWarmQ.lt, 1),
                "mm")))
  })
  
  output$precip_cold <- renderUI({
    validate(need(ltclim_fordisplay()$PrecColdQ.lt, ""))
    tags$b(class = "bodylarge",
           style = paste("color:", appcolors[["Dark Green"]], ";"),
    HTML(paste0(round(ltclim_fordisplay()$PrecColdQ.lt, 1),
                "mm")))
  })
    
  #text for AnnPrec region
  output$annprec.lt.region <- renderText({
    validate(need(selected_region(), ""))
    climate_row <- which(climate.lt$label == selected_region())
    sprintf("Long term average for %s: %imm",
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

  observeEvent(input$save, {
    session$doBookmark()
  }, ignoreInit = FALSE, priority = -100)  
 
  outofmodule
    }
  )
}

app_selectlocation <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {bootstrapPage(
      shinyjs::useShinyjs(),
    tags$head(tags$style(appcss),
	      tags$link(href="https://fonts.googleapis.com/css?family=Poppins|Inter", rel="stylesheet"),
              includeHTML("./www/extra.html"),
	      ),
      fluidRow(selectlocationUI("location")),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region_outer <- reactiveVal()
      AnnPrec.YfA_outer <- reactiveVal()
      refresh <- reactiveTimer(1000 * 5)
      observeEvent(refresh(), {
        selected_region_outer(NULL)
        selected_region_outer("")
        showNotification("Region to empty")
      })
      selectlocationServer("location", selected_region_outer, AnnPrec.YfA_outer)
    })
}
