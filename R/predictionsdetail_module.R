predictionsdetailUI <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("species_InModal"), height = "800px")
  )
}

 
predictionsdetailServer <- function(id,
                              data){
  moduleServer(
    id,
    function(input, output, session){
        output$species_InModal <- plotly::renderPlotly({
          species_plotly_modal(data$species_prob_current, data$spec_different)
      })
    })
  }