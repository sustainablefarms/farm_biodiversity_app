predictionsdetailUI <- function(id, speciesinfo_topten, speciesinfo_botten){
  ns <- NS(id)
  fluidPage(
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
  column(6, 
    HTML("<div class='subheader'><h2>OCCUPANCY PROBABILITY OF ALL SPECIES</h2></div>"),
    fluidRow(
      plotOutput(ns("allspecies"), height = "800px")
    ),
    tags$div(class='subheader',
	      tags$h2("OCCUPANCY PROBABILITY RELATIVE TO REFERENCE",
                  infotooltip(HTML("This is the ratio of each species' estimated occupancy probability to the reference occupancy probability.",
				   "For example, if the Superb Parrot has a ratio of '2', then it is estimated that the Superb Parrot is twice as likely to live in your farm's woodland than in the reference farm.",
				   "<br><br>",
				   "A ratio of '1' means the species is <em>equally</em> as likely to occupy your farm's woodland as the reference farm.",
				   "A ratio smaller than 1 means the species is <em>less</em> likely to occupy your farm's woodland than the reference farm.")))),
    fluidRow(
      plotOutput(ns("allspeciesrel"), height = "800px")
    )
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
    lapply(consstatus$CommonName, function(specname) vulnerablespecUI(ns, specname)),
  ) 
  )
}

 
predictionsdetailServer <- function(id,
                              data){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
      lapply(consstatus$CommonName, function(specname){
        output[[gsub("(-| )", "", specname)]] <- renderText({
          c("The", specname, consstatus[specname, "statussummary"],
          onespecwords(specname, data$species_prob_current))
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
