# select YfA info# select location
selectYfAUI <- function(id){
  ns <- NS(id)
  tagList(
         HTML("<div class='subheader'><h2>RAINFALL SINCE LAST AUGUST</h2></div>"),
        tags$div("Approximate Rainfall Since Last August (mm)"),
        sliderInput(
          inputId = ns("AnnPrec.YfA"),
          label = NULL,
          min = 400, max = 1000, step = 20,
	  width = "100%",
          value = new_data_mean$AnnPrec.YfA),
       tags$div(textOutput(ns("annprec.lt.region"), inline = TRUE))
  )
}



selectYfAServer <- function(id, locationinfo){
  moduleServer(
    id,
    function(input, output, session){
      stopifnot(is.reactive(locationinfo))
      yfabookmark <- reactiveValues(# for making sure the yfa bookmarked value get update rather than the long-term value
        usebookmark = FALSE,
        value = NULL
      )
      observeEvent(locationinfo()$AnnPrec.lt, {
	validate(need(locationinfo()$AnnPrec.lt, ""))
	updateSliderInput(inputId = "AnnPrec.YfA",
			  value = locationinfo()$AnnPrec.lt)
	showNotification(paste("YfA from lt:", locationinfo()$AnnPrec.lt))
      }, priority = 100, ignoreInit = TRUE, ignoreNULL = TRUE)
      
      outOfModule <- reactive({
        out <- list()
        out$AnnPrec.YfA <- input$AnnPrec.YfA
        out
      }) %>% throttle(1000)
      
      output$annprec.lt.region <- renderText({
        validate(need(locationinfo()$selected_region, ""))
        paste0("(long term average for ", locationinfo()$selected_region, ": ", locationinfo()$AnnPrec.lt , "mm",
                ")")
      })
      
      # setBookmarkExclude(c("AnnPrec.YfA"))
      
      outOfModule
    }
  )
}
