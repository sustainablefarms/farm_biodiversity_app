# select YfA info# select location
selectYfAUI <- function(id){
  ns <- NS(id)
  twocolumns(heading = "Approximate recent rainfall",
             left = tagList(tags$p(class = "bodysmall",
             "Some species show a small sensitivity to the rainfall since the start of last spring.",
             "The sensitivity is small so only a rough estimate of rainfall is required.",
             "Often using the average annual rainfall for your region will be good enough."),
             infotext("Drag to adjust rainfall amount")),
             right = tagList(
               sliderInput(
                 inputId = ns("AnnPrec.YfA"),
                 label = NULL,
                 min = 400, max = 1000, step = 20,
                 width = "100%",
                 value = new_data_mean$AnnPrec.YfA),
               tags$div(textOutput(ns("annprec.lt.region"), inline = TRUE))
             )
  )
}



selectYfAServer <- function(id, selected_region, inAnnPrec.YfA){
  moduleServer(
    id,
    function(input, output, session){
      stopifnot(is.reactive(selected_region))
      yfabookmark <- reactiveValues(# for making sure the yfa bookmarked value get update rather than the long-term value
        usebookmark = FALSE,
        value = NULL
      )
      climate.lt <- readRDS("data/sa2_points_climate.rds")
  # update YfA based on new location info
      observeEvent(selected_region(), {
      	validate(need(selected_region(), ""))
        climate_row <- which(climate.lt$label == selected_region())
	updateSliderInput(inputId = "AnnPrec.YfA",
			  value = climate.lt$AnnPrec[climate_row])
      }, priority = 100, ignoreInit = TRUE, ignoreNULL = TRUE)
  # whenever both inputs change at the same time, do an update from inAnnPrec.YfA *last*
  observeEvent(inAnnPrec.YfA(), {
    validate(need(inAnnPrec.YfA(), ""))
    updateSliderInput(inputId = "AnnPrec.YfA",
                        value = inAnnPrec.YfA())
    }, priority = 80, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      outOfModule <- reactive({
        out <- list()
        out$AnnPrec.YfA <- input$AnnPrec.YfA
        out
      }) %>% throttle(1000)
      
      output$annprec.lt.region <- renderText({
        validate(need(selected_region(), ""))
        climate_row <- which(climate.lt$label == selected_region())
        sprintf("(long term average for %s: %i)",
                selected_region(),
                climate.lt$AnnPrec[climate_row])
      })
      
      # bookmarking
      observe({
        outOfModule()
        session$doBookmark()
      })
      
      outOfModule
    }
  )
}

app_selectYfA <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {bootstrapPage(
      includeCSS("./www/base.css"),
      selectYfAUI("yfa"),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region <- reactiveVal("Albury Region")
      inAnnPrec.YfA <- reactiveVal(400)
      refresh <- reactiveTimer(1000 * 10)
      observeEvent(refresh(), {
        inAnnPrec.YfA(inAnnPrec.YfA() * 1.1)
      })
      selectYfAServer("yfa", selected_region, inAnnPrec.YfA)
    }
  )
}
