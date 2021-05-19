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
    fluidRow(
      specimageOut(speciesinfo["Superb Parrot", ],
                   height = "100px"),
      tags$div(
        tags$br(),
        "The Superb Parrot is listed as vulnerable federally",
        # linknewtab(href = "https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl", "federally,"),
        "and in Victoria, and is listed as threatened in NSW.",
        textOutput(ns("superbparrotprobdesc"), inline = TRUE)
      )
    ),
    fluidRow(
      specimageOut(speciesinfo["Dusky Woodswallow", ],
                   height = "100px"),
      tags$div(
        tags$br(),
        "The Dusky Woodswallow is listed as vulnerable in NSW.",
        textOutput(ns("DuskyWoodswallowprobdesc"), inline = TRUE)
      )
    ),
    HTML("<div><plottitle>Brown Treecreeper</<plottitle></div>"),
    tags$div(
      "The Brown Treecreeper is listed as vulnerable in NSW.",
      textOutput(ns("BrownTreecreeperprobdesc"), inline = TRUE)
    ),
    HTML("<div><plottitle>Grey-crowned Babbler</<plottitle></div>"),
    tags$div(
      "The Grey-crowned Babbler is listed as vulnerable in NSW and threatened in Victoria.",
      textOutput(ns("GreycrownedBabblerprobdesc"), inline = TRUE)
    ),
    HTML("<div><<plottitle>Diamond Firetail</<plottitle></div>"),
    tags$div(
      "The Diamond Firetail is listed as vulnerable in NSW and threatened in Victoria.",
      textOutput(ns("DiamondFiretailprobdesc"), inline = TRUE)
    ),
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
      
      output$superbparrotprobdesc <- renderText({
        onespecwords("Superb Parrot", data$species_prob_current)
      }, sep = "")
      output$DuskyWoodswallowprobdesc <- renderText({
        onespecwords("Dusky Woodswallow", data$species_prob_current)
      }, sep = "")
      output$BrownTreecreeperprobdesc <- renderText({
        onespecwords("Brown Treecreeper", data$species_prob_current)
      }, sep = "")
      output$GreycrownedBabblerprobdesc <- renderText({
        onespecwords("Grey-crowned Babbler", data$species_prob_current)
      }, sep = "")
      output$DiamondFiretailprobdesc <- renderText({
        onespecwords("Diamond Firetail", data$species_prob_current)
      }, sep = "")
      
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
      
      output$allspeciesrel <- renderPlot({
        plot_allspeciesrel(data$spec_different)
      })
    })
  }
