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



selectYfAServer <- function(id, locationinfo, inAnnPrec.YfA){
  moduleServer(
    id,
    function(input, output, session){
      stopifnot(is.reactive(locationinfo))
      yfabookmark <- reactiveValues(# for making sure the yfa bookmarked value get update rather than the long-term value
        usebookmark = FALSE,
        value = NULL
      )
  # update YfA based on new location info
      observeEvent(locationinfo()$AnnPrec.lt, {
	validate(need(locationinfo()$AnnPrec.lt, ""))
	updateSliderInput(inputId = "AnnPrec.YfA",
			  value = locationinfo()$AnnPrec.lt)
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
        validate(need(locationinfo()$selected_region, ""))
        paste0("(long term average for ", locationinfo()$selected_region, ": ", locationinfo()$AnnPrec.lt , "mm",
                ")")
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
      locationinfo = reactiveVal(list(
        selected_region = "Goulburn",
        AnnPrec.lt = 600
      ))
      inAnnPrec.YfA <- reactiveVal(400)
      refresh <- reactiveTimer(1000 * 10)
      observeEvent(refresh(), {
        inAnnPrec.YfA(inAnnPrec.YfA() * 1.1)
      })
      selectYfAServer("yfa", locationinfo, inAnnPrec.YfA)
    }
  )
}
