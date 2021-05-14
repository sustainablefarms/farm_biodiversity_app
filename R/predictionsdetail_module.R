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
        tags$div(speciesinfo_topten[idx, "species"],
                  style = "float: left",
                   tags$br(),
        linknewtab(
          href = speciesinfo_topten[idx, "url"],
          style="text-align: center",
          imageOutput(ns(paste0("top", idx)), height = "200px", inline = TRUE),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          `data-viewport` = "{'selector': '#topten5'}",
          title = speciesinfo_topten[idx, "story"]),
        "Creator: = ???",
        linknewtab(href="https://creativecommons.org/licenses/by-nc-sa/3.0/",
                   tags$img(src = "https://licensebuttons.net/l/by-nc-sa/3.0/88x31.png",
                            alt = "CC BY-NC-SA 3.0",
                            height = "20px")
          )
        )
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
    
    HTML("<div class='subheader'><h2>A VULNERABLE SPECIES: SUPERB PARROT</h2></div>"),
    tags$div(
    "The Superb Parrot is listed as vulnerable by the ",
    linknewtab(href = "https://www.environment.gov.au/cgi-bin/sprat/public/publicthreatenedlist.pl", "Commonwealth Government."),
    textOutput(ns("superbparrotprobdesc"), inline = TRUE)
    ),
    HTML("<div class='subheader'><h2>OCCUPANCY PROBABILITY OF ALL SPECIES</h2></div>"),
    fluidRow(
      plotOutput(ns("allspecies"), height = "800px")
    ),
    HTML("<div class='subheader'><h2>OCCUPANCY PROBABILITY RELATIVE TO REFERENCE</h2></div>"),
    fluidRow(
      plotOutput(ns("allspeciesrel"), height = "800px")
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
          "%) chance of the Superb Parrot occupying at least one of your woodland patches."
        )
      }, sep = "")
      
      output$allspecies <- renderPlot({
        plot_allspeciesprob(data$species_prob_current)
      })
      
      output$allspeciesrel <- renderPlot({
        plot_allspeciesrel(data$spec_different)
      })
    })
  }
