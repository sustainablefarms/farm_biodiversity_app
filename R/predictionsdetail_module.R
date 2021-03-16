predictionsdetailUI <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("species_InModal"), height = "800px")
  )
}

predictionsdetailServer <- function(id, 
                              current_values,
                              model_data,
                              new_data_mean,
                              report_path){
  moduleServer(
    id,
    function(input, output, session){
      output$species_InModal <- plotly::renderPlotly({
        species_plotly_modal(data$species_prob_current, data$spec_different)
      })
    })
  }