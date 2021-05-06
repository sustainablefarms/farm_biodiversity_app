# select YfA info# select location
selectYfAUI <- function(id){
  ns <- NS(id)
  tagList(
         HTML("<div class='subheader'><h2>SINCE LAST AUGUST</h2></div>"),
        tags$div("Rainfall (mm)"),
        sliderInput(
          inputId = ns("AnnPrec.YfA"),
          label = NULL,
          min = 450, max = 1050, step = 10,
          value = new_data_mean$AnnPrec.YfA),
       tags$div(textOutput(ns("annprec.lt.region"), inline = TRUE))
  )
}



selectYfAServer <- function(id, locationinfo){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      outOfModule <- reactiveValues(
        # AnnMeanTemp.YfA        = new_data_mean$AnnMeanTemp.YfA,
        AnnPrec.YfA            = new_data_mean$AnnPrec.YfA
        # MaxTWarmMonth.YfA      = new_data_mean$MaxTWarmMonth.YfA,
        # PrecWarmQ.YfA          = new_data_mean$PrecWarmQ.YfA,
        # MinTColdMonth.YfA      = new_data_mean$MinTColdMonth.YfA,
        # PrecColdQ.YfA          = new_data_mean$PrecColdQ.YfA,
        # PrecSeasonality.YfA    = new_data_mean$PrecSeasonality.YfA
      )
      observe({
        # outOfModule$AnnMeanTemp.YfA <- input$AnnMeanTemp.YfA
        outOfModule$AnnPrec.YfA <- input$AnnPrec.YfA
      })
      
      output$annprec.lt.region <- renderText({
        validate(need(locationinfo$selected_region, ""))
        paste0("(long term average for ", locationinfo$selected_region, ": ", locationinfo$AnnPrec.lt , "mm",
                ")")
      })
      
      outOfModule
    }
  )
}