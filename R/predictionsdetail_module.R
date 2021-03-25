predictionsdetailUI <- function(id, speciesinfo_topten){
  ns <- NS(id)
  fluidPage(
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    # the following enables bootstrap 3's inbuilt popovers
    do.call(fluidRow, 
      lapply(1:5, function(idx){
        tags$div(
          imageOutput(ns(paste0("top", idx))),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          title = speciesinfo_topten[idx, "story"])
        }
      )),
    fluidRow(
      tags$div(
        imageOutput(ns("top6")),
        `data-toggle` = "tooltip",
        `data-placement` = 'auto bottom',
        title = speciesinfo_topten[6, "story"]
      )
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
      lapply(1:10, function(idx){
        output[[paste0("top", idx)]] <- renderImage({
            list(src = data$speciesinfo_topten[idx, "imgfilename"],
                 alt = data$speciesinfo_topten[idx, "species"])
          }, deleteFile = FALSE, quoted = FALSE)
      })
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
    })
  }