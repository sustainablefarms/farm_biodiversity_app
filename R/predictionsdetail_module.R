predictionsdetailUI <- function(id, speciesinfo_topten){
  ns <- NS(id)
  fluidPage(
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    # the following enables bootstrap 3's inbuilt popovers
    fluidRow(
      id = ns("topten5"),
      style="text-align: center",
      lapply(1:5, function(idx){
        column(2, 
        tags$span(
          style="text-align: center",
          imageOutput(ns(paste0("top", idx)), height = "100px", inline = TRUE),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          `data-viewport` = "{'selector': '#topten5'}",
          title = speciesinfo_topten[idx, "story"])
        )
        }
      )),
    fluidRow(
      style="text-align: center",
      lapply(6:10, function(idx){
        column(2, 
               tags$div(
                 style="text-align: center",
                 imageOutput(ns(paste0("top", idx)), height = "100px", inline = FALSE),
                 `data-toggle` = "tooltip",
                 `data-placement` = 'auto bottom',
                 `data-viewport` = "{'selector': ':root'}",
                 title = speciesinfo_topten[idx, "story"])
        )
      })
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
                 alt = data$speciesinfo_topten[idx, "species"],
                 height = "100px")
          }, deleteFile = FALSE, quoted = FALSE)
      })
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
    })
  }