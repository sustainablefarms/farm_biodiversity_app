# select YfA info# select location
selectYfAUI <- function(id){
  ns <- NS(id)
  tagList(
         HTML("<div class='subheader'><h2>Since Last August</h2></div>"),
         column(width = 6,
                sliderInput(
                  inputId = ns("AnnMeanTemp.YfA"),
                  label = tags$html(tags$span(HTML("Average temperature (&deg;C)"))),
                  min = 13, max = 17, step = 0.5,
                  value = new_data_mean$AnnMeanTemp.YfA)),
         column(width = 6,
                sliderInput(
                  inputId = ns("AnnPrec.YfA"),
                  label = tags$html(tags$span("Rainfall (mm)")),
                  min = 450, max = 1050, step = 10,
                  value = new_data_mean$AnnPrec.YfA))
        )
}



selectYfAServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      # set up reactive values
      outOfModule <- reactiveValues(
        AnnMeanTemp.YfA        = new_data_mean$AnnMeanTemp.YfA,
        AnnPrec.YfA            = new_data_mean$AnnPrec.YfA,
        MaxTWarmMonth.YfA      = new_data_mean$MaxTWarmMonth.YfA,
        PrecWarmQ.YfA          = new_data_mean$PrecWarmQ.YfA,
        MinTColdMonth.YfA      = new_data_mean$MinTColdMonth.YfA,
        PrecColdQ.YfA          = new_data_mean$PrecColdQ.YfA,
        PrecSeasonality.YfA    = new_data_mean$PrecSeasonality.YfA
      )
      observe({
        outOfModule$AnnMeanTemp.YfA <- input$AnnMeanTemp.YfA
        outOfModule$AnnPrec.YfA <- input$AnnPrec.YfA
      })
      outOfModule
    }
  )
}