predictionsdetailUI <- function(id, speciesinfo_topten, speciesinfo_botten){
  ns <- NS(id)
  fluidPage(
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    HTML("<div class='subheader'><h2>10 MOST LIKELY BIRDS</h2></div>"),
    fluidRow(
      id = ns("topten5"),
      style="text-align: center",
      lapply(1:5, function(idx){
        # column(2, 
        linknewtab(
          href = speciesinfo_topten[idx, "url"],
          style="text-align: center",
          imageOutput(ns(paste0("top", idx)), height = "200px", inline = TRUE),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          `data-viewport` = "{'selector': '#topten5'}",
          title = speciesinfo_topten[idx, "story"])
        # )
        }
      )),
    fluidRow(
      style="text-align: center",
      lapply(6:10, function(idx){
        # column(2, )
        linknewtab(
         href = speciesinfo_topten[idx, "url"],
         style="text-align: center",
         imageOutput(ns(paste0("top", idx)), height = "200px", inline = TRUE),
         `data-toggle` = "tooltip",
         `data-placement` = 'auto bottom',
         `data-viewport` = "{'selector': ':root'}",
         title = speciesinfo_topten[idx, "story"])
        # )
      })
    ),
    HTML("<div class='subheader'><h2>10 LEAST LIKELY BIRDS</h2></div>"),
    fluidRow(
      style="text-align: center",
      lapply(1:10, function(idx){
        # column(2, )
        linknewtab(
          href = speciesinfo_botten[idx, "url"],
          style="text-align: center",
          imageOutput(ns(paste0("bot", idx)), height = "100px", inline = TRUE),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto bottom',
          `data-viewport` = "{'selector': ':root'}",
          title = speciesinfo_botten[idx, "story"])
        # )
      })
    ),
    
    HTML("<div class='subheader'><h2>A Vulnerable Species: Superb Parrot</h2></div>"),
    tags$div(
    "The Superb Parrot is listed as vulnerable by the ",
    linknewtab(href = "https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl", "Commonwealth Government."),
    textOutput(ns("superbparrotprobdesc"), inline = TRUE)
    ),
    HTML("<div class='subheader'><h2>OCCUPANCY PROBABILITY OF ALL SPECIES</h2></div>"),
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
                 height = "200px")
          }, deleteFile = FALSE, quoted = FALSE)
      })
      
      lapply(1:10, function(idx){
        output[[paste0("bot", idx)]] <- renderImage({
            list(src = data$speciesinfo_botten[idx, "imgfilename"],
                 alt = data$speciesinfo_botten[idx, "species"],
                 height = "100px")
          }, deleteFile = FALSE, quoted = FALSE)
      })
      
      output$superbparrotprobdesc <- renderText({
        c(
          "According to our model, there is a ",
          format(data$species_prob_current["Superb Parrot", "median"] * 100, digits = 2),
          "% (lower bound = ",
          format(data$species_prob_current["Superb Parrot", "lower"] * 100, digits = 2),
          "%), upper bound = ",
          format(data$species_prob_current["Superb Parrot", "upper"] * 100, digits = 2),
          "%) chance of the Superb Parrot occupying patch ",
          data$species_prob_current["Superb Parrot", "bestsite"],
          ", which was the best patch for the Superb Parrot."
        )
      }, sep = "")
      
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
    })
  }
