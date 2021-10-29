# predictions module
predictionsUI <- function(id, refisaverage = TRUE){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    # update species images after plots finished
    tags$script("
      Shiny.addCustomMessageHandler('plotfinished', function(state){
       $('.specimg').each(function(index){$( this ).attr('src', $( this ).attr('data-src'))});
      });
    "),
    plotly::plotlyOutput(ns("plotlybug"), height = "0px"),
    tags$h4("Expected Number of Species"),
    twocolumns(heading = NULL,
               left = tagList(paste("The <em>second</em> bar is the expected number of birds species in our model that we predict will be occupying at least one patch on your farm.",
                            "<br><br>",
                            "The top bar is the number of species we expect if there is 2% (1.5ha) nearby woody cover for every patch.",
                            "The third bar is the number of species we expect if there is 20% (15ha) nearby woody cover for every patch.",
                            "The final bar is the number of species we expect from your reference estimates.",
                            "<br><br>Each species was assigned an occupancy probability equal to the maximum of all patches (we use the maximum as we expect occupancy between patches to be highly correlated)."),
                            tags$em(uiOutput(ns("warn"), inline = TRUE))),
               right = plotOutput(ns("species_richness"), height = "250px")
    ),
    accordion(ns("predacc"),
              accordion_item(title = "Most likely species", id = ns("mostlikely"),
               twocolumns(heading = "The 10 most likely species to live in your farm's Box Gum Grassy Woodland.",
                          left = proboccplotdescription,
                          right = tagList(
                            mostlikely_plot_UI(ns("mlp"), refisaverage = refisaverage),
                            tags$div(style="text-align: center",
                                     uiOutput(ns("mostlikelyspecimages")))
                            )
                         )
                ),
              accordion_item(title = "Least likely species", id = ns("leastlikely"),
                twocolumns(heading = "The 10 least likely species to live in your farm's Box Gum Grassy Woodland.",
                           left = tags$p("Of the species in", appname, "these species are least likely. This doesn't include rare birds not in", appname, "."),
                           right = tags$div(
                             style="text-align: center",
                             uiOutput(ns("leastlikespecimages"))
                           )
                 )
                ),
              accordion_item(title = "Vulnerable species", id = ns("vulspec"),
                twocolumns(heading = "Heading",
                           left = "about",
                           right = lapply(consstatus$CommonName, function(specname) vulnerablespecUI(ns, specname)))
                ),
              accordion_item(title = "Occupancy Probability of All Species", id = ns("occall"),
                twocolumns(heading = "Estimates of the occupancy probability for every species",
                           left = tagList(proboccplotdescription,
                                          tags$p("Body length data from",
                                                 linknewtab(href = "https://www.nature.com/articles/sdata201561", "Garnett et al. (Scientific Data 2, 2015)."))),
                           right = plotOutput(ns("allspecies"), height = "800px")
                           )
                ),
              accordion_item(title = "Relative Occupancy Probability", id = ns("occallrel"),
                             twocolumns(heading = "Estimates of occupancy probability relative to S1 for every species.",
                                        left = tags$div(tags$p("This is the ratio of each species' estimated occupancy probability to the reference occupancy probability.",
                                                               "For example, if the Superb Parrot has a ratio of '2', then it is estimated that the Superb Parrot is twice as likely to live in your farm's woodland than in the reference farm."),
                                                        tags$p(
                                                          "A ratio of '1' means the species is", tags$em("equally"), "as likely to occupy your farm's woodland as the reference farm.",
                                                          "A ratio smaller than 1 means the species is", tags$em("less"), "likely to occupy your farm's woodland than the reference farm."),
                                                        tags$p("Body length data from",
                                                               linknewtab(href = "https://www.nature.com/articles/sdata201561", "Garnett et al. (Biological, ecological, conservation and legal information for all species and subspecies of Australian bird. Scientific Data 2, 2015)."))
                                                      ),
                                        right = plotOutput(ns("allspeciesrel"), height = "800px")
                             )
              )
              ),
    
    
      fluidRow(
        column(width = 6,
          if (isTRUE(getOption("shiny.testmode"))){
            downloadButton(ns("downloaddataverbose"), "Verbose Prediction Data", class = "download_badge")
          },
          downloadButton(ns("downloaddata"), "Table", class = "download_badge"),
          downloadButton(ns("downloadreport"), "Report", class = "download_badge")
               ),
        column(width = 5, offset = 1,
          # style = "text-align: right",
            "Reference:",
          infotooltip(title = tags$html(tags$em("Comparing to Other Estimates"),
				  referencesblurb,
                                   tags$p(tags$em("Update"), "sets the reference estimates to the current estimates.",
                                   "After setting the reference estimates for the first time, uncheck", tags$em("Use Default"), "to use them.",
                                   "Initially the ratio of occupancy probabilities to reference probabilities will be all 1.",
                                   "This will be the case until you alter attributes of the farm's woodland."),
                                   tags$p(tags$em("Use Default,"), "when checked, overides the saved estimates with the default estimates.",
                                   "Uncheck", tags$em("Use Default"), "to use the saved reference."),
				   tags$p(tags$em("Warning: refreshing the web browser removes the saved reference."))),
		      HTML = TRUE)
               )
      )
  )
}

predictionsServer <- function(id, 
                              current_values,
                              refpredictions,
                              model_data,
                              report_path,
                              refisaverage = TRUE){
  moduleServer(
    id,
    function(input, output, session){
      data <- reactiveValues(
        Xocc = NULL,
        species_prob_current = NULL,
        species_richness = NULL,
        toptennames = NULL,
        speciesinfo_topten = NULL,
        speciesinfo_botten = NULL)
      moredetailopens <- reactiveVal(value = 0, label = "moredetailopens")
      ns <- session$ns
  
      # compute predictions below
      datar <- reactive({
        data <- compile_predictions(current_values(), refpredictions(), refisaverage = refisaverage)
        session$sendCustomMessage("predictionsmade", "nothing") #for usage tracking
        data
      })
      
      # draw species plots
      mostlikely_plot_Server("mlp", 
                             reactive({datar()$species_prob_current}),
                             refpredictions
                             )
        
      
      # draw species richness
      output$species_richness <- renderPlot({
        validate(need(datar()$species_richness, ""))
        richness_plot(datar()$species_richness)
      })
      
      # species images
      output$leastlikespecimages <- renderUI({
        validate(need(datar()$speciesinfo_botten, ""))
        lapply(10:1, function(idx) specimageOut(datar()$speciesinfo_botten[idx, ],
                                                height = "100px"))
      })
      
      output$mostlikelyspecimages <- renderUI({
        validate(need(datar()$speciesinfo_topten, ""))
        lapply(1:10, function(idx) specimageOut(datar()$speciesinfo_topten[idx, ],
                              height = "100px"))
      })
      
      #vulnerable species
      lapply(consstatus$CommonName, function(specname){
        output[[gsub("(-| )", "", specname)]] <- renderText({
          validate(need(datar()$species_prob_current, ""))
          c("The", specname, consstatus[specname, "statussummary"],
            onespecwords(specname, datar()$species_prob_current),
            onespecwords(specname, refpredictions())
            )
        })
      })
      
      # All species plots
      wprob <- waiter::Waiter$new(id = ns("allspecies"))
      output$allspecies <- renderPlot({
        wprob$show()
        on.exit(wprob$hide())
        on.exit(session$sendCustomMessage("plotfinished", TRUE))
        plot_allspeciesprob(datar()$species_prob_current)
      })
      
      wrel <- waiter::Waiter$new(id = ns("allspeciesrel"))
      output$allspeciesrel <- renderPlot({
        wrel$show()
        on.exit(wrel$hide())
        on.exit(session$sendCustomMessage("plotfinished", TRUE))
        plot_allspeciesrel(datar()$spec_different)
      })
      
      output$downloaddata <- downloadHandler(
        filename = "predictions.csv",
        content = function(file) {
          outdata <- datar()$species_prob_current
          outdata <- cbind(Species = rownames(outdata), as.data.frame(outdata))
          colnames(outdata)[colnames(outdata) == "median"] <- "Predicted Probability"
          colnames(outdata)[colnames(outdata) == "bestsite"] <- "Patch"
          write.csv(outdata, file, row.names = FALSE)
        })
      
      output$downloadreport <- downloadHandler(
        filename = "report.pdf",
        content = function(file) {
          id <- showNotification(
            "Rendering report...",
            duration = NULL,
            closeButton = FALSE
          )
          on.exit(removeNotification(id), add = TRUE)
          rmarkdown::render(input = report_path, 
                            params = list(loadexampledata = FALSE),
                            output_file = file,
                            envir = new.env(parent = environment())
          )
        }
      )
      
      output$warn <- renderUI({
        validate(need(current_values()$locationcomplete & current_values()$allpatchcomplete, ""))
        warn <- warn_oot(current_values())
        validate(need(warn[["warn"]], ""))
        tagList(
          tags$script("$(function () {
                        $('[data-toggle=tooltip]').tooltip()
                      })"
          ),
          tags$span(style = "color:red;", "Warning",
                   infotooltip(warn[["reason"]]))
        )
      })
      
      if (isTRUE(getOption("shiny.testmode"))){
        output$downloaddataverbose <- downloadHandler(
          filename = "predictions.rds",
          content = function(file) {
            saveRDS(datar(), file)
          },
          contentType = "rds")
      }
      
      # bookmarking settings
      setBookmarkExclude(c("moredetail"))
      reactive(datar()$species_prob_current)
    })
}

# for running predictions module standalone (I think reference will not work though)
app_predictions <- function(){
  main_app_prep()
  current_values <- reactiveVal(value = readRDS("./current_values.rds")) 
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      predictionsUI("pred", refisaverage = FALSE),
      theme = bslib::bs_theme(version = 5, "lumen"))
      },
           function(input, output, session){
             predictionsServer("pred", 
                               current_values,
                               reactiveVal(species_prob_mean),
                               model_data, 
                               report_path,
                               refisaverage = FALSE)
           })
}
