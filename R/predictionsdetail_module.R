predictionsdetailUI <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      plotOutput(ns("allspecies"), height = "800px")
    )
  )
}

 
predictionsdetailServer <- function(id,
                              data){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
    })
  }