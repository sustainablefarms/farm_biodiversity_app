predictionsdetailUI <- function(id, speciesinfo_topten, speciesinfo_botten){
  ns <- NS(id)
  fluidPage(
    waiter::use_waiter(),
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
     ),
    # an update species after plots created
    tags$script("
      Shiny.addCustomMessageHandler('plotfinished', function(state){
       $('.specimg').each(function(index){$( this ).attr('src', $( this ).attr('data-src'))});
      });
    "),
  column(6, 
    tags$div(class='subheader',
	     tags$h2("OCCUPANCY PROBABILITY OF ALL SPECIES",
                  infotooltip(title = tags$html(tags$p("Estimates of the occupancy probability for every species, arranged by body length."),
					   proboccplotdescription,
				tags$p("Body length data from",
			             linknewtab(href = "https://www.nature.com/articles/sdata201561", "Garnett et al. (Scientific Data 2, 2015)."))
		     ), placement = "bottom"))),
    fluidRow(
      plotOutput(ns("allspecies"), height = "800px")
    ),
    tags$div(class='subheader',
	      tags$h2("OCCUPANCY PROBABILITY RELATIVE TO REFERENCE",
                  infotooltip(tags$div(tags$p("This is the ratio of each species' estimated occupancy probability to the reference occupancy probability.",
				   "For example, if the Superb Parrot has a ratio of '2', then it is estimated that the Superb Parrot is twice as likely to live in your farm's woodland than in the reference farm."),
				   tags$p(
				   "A ratio of '1' means the species is", tags$em("equally"), "as likely to occupy your farm's woodland as the reference farm.",
				   "A ratio smaller than 1 means the species is", tags$em("less"), "likely to occupy your farm's woodland than the reference farm."),
				tags$p("Body length data from",
			             linknewtab(href = "https://www.nature.com/articles/sdata201561", "Garnett et al. (Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data 2, 2015)."))
				   )))),
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
                              data, modalopens){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      wprob <- waiter::Waiter$new(id = ns("allspecies"))
      wrel <- waiter::Waiter$new(id = ns("allspeciesrel"))
      
      lapply(consstatus$CommonName, function(specname){
        output[[gsub("(-| )", "", specname)]] <- renderText({
          c("The", specname, consstatus[specname, "statussummary"],
          onespecwords(specname, data$species_prob_current))
        })
      })
      
      output$allspecies <- renderPlot({
	wprob$show()
	on.exit(wprob$hide())
        on.exit(session$sendCustomMessage("plotfinished", TRUE))
        plot_allspeciesprob(data$species_prob_current)
      })
      

      output$allspeciesrel <- renderPlot({
	modalopens()
	wrel$show()
	on.exit(wrel$hide())
        on.exit(session$sendCustomMessage("plotfinished", TRUE))
        plot_allspeciesrel(data$spec_different)
      })
    })
  }
