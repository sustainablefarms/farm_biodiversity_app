predictionsdetailUI <- function(id, speciesinfo_topten, speciesinfo_botten){
  ns <- NS(id)
  fluidPage(
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
  column(6,
    HTML("<div class='subheader'><h2>10 MOST LIKELY BIRDS</h2></div>"),
    fluidRow(
      id = ns("topten5"),
      style="text-align: center",
      lapply(1:10, function(idx) specimageOut(
        speciesinfo_topten[idx, ],
        height = "100px"))
      ),
    HTML("<div class='subheader'><h2>10 LEAST LIKELY BIRDS</h2></div>"),
    fluidRow(
      style="text-align: center",
      lapply(10:1, function(idx) specimageOut(speciesinfo_botten[idx, ],
                                             height = "100px"))
    ),
    HTML("<div class='subheader'><h2>VULNERABLE SPECIES</h2></div>"),
    vulnerablespecUI(ns, "Superb Parrot",
                     constatuswords = paste("is listed as vulnerable federally",
                     "and in Victoria, and is listed as threatened in NSW.")
                     ),
    vulnerablespecUI(ns, "Dusky Woodswallow",
                     constatuswords = paste("is listed as vulnerable in NSW.")
                     ),
    vulnerablespecUI(ns, "Brown Treecreeper",
                     "is listed as vulnerable in NSW."),
    vulnerablespecUI(ns, "Grey-crowned Babbler",
                        "is listed as vulnerable in NSW and threatened in Victoria."),
    vulnerablespecUI(ns, "Diamond Firetail",
                     "is listed as vulnerable in NSW and threatened in Victoria."),
  ), 
  column(6, 
    HTML("<div class='subheader'><h2>OCCUPANCY PROBABILITY OF ALL SPECIES</h2></div>"),
    fluidRow(
      plotOutput(ns("allspecies"), height = "800px")
    ),
    HTML("<div class='subheader'><h2>OCCUPANCY PROBABILITY RELATIVE TO REFERENCE</h2></div>"),
    fluidRow(
      plotOutput(ns("allspeciesrel"), height = "800px")
    )
  )
  )
}

 
predictionsdetailServer <- function(id,
                              data){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      constatspecies <- readRDS("./data/consstatus.rds")
      lapply(constatspecies$CommonName, function(specname){
        output[[gsub("(-| )", "", specname)]] <- renderText({
          onespecwords(specname, data$species_prob_current)
        })
      })
      
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
      
      output$allspeciesrel <- renderPlot({
        plot_allspeciesrel(data$spec_different)
      })
    })
  }
