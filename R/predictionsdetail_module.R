predictionsdetailUI <- function(id, toptennames){
  print(toptennames)
  ns <- NS(id)
  fluidPage(
    fluidRow(
      tags$img(src = speciesinfo[1, "imgfilename"],
               width=100,
               height=100)
    ),
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