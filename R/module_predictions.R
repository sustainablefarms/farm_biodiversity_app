# predictions module
predictionsUI <- function(id){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    tags$p(class = "alignleft",
           HTML("<h4>Expected Number of Species</h4>"),
           infotooltip(title = paste("The <em>second</em> bar is the expected number of birds species in our model that we predict will be occupying at least one patch on your farm.",
                                     "<br><br>",
                                     "The top bar is the number of species we expect if there is 2% (1.5ha) nearby woody cover for every patch.",
                                     "The third bar is the number of species we expect if there is 20% (15ha) nearby woody cover for every patch.",
                                     "The final bar is the number of species we expect from your reference estimates.",
                                     "<br><br>Each species was assigned an occupancy probability equal to the maximum of all patches (we use the maximum as we expect occupancy between patches to be highly correlated)."
           ),
           placement = "bottom")
    ),
    tags$p(class =  "alignright",
           tags$em(uiOutput(ns("warn"), inline = TRUE))),
    tags$div(style="clear: both;"),
    plotOutput(ns("species_richness"), height = "250px"),
    accordion(ns("predacc"),
              accordion_item(title = "Most likely species", id = ns("mostlikely"),
                             fluidRow(
                               column(width = 4,
                                      tags$html(tags$p("The 10 most likely species to live in your farm's Box Gum Grassy Woodland according to our model."),
                                                proboccplotdescription)
                                      ),
                               column(width = 8,
                                      plotly::plotlyOutput(ns("common_species"), height = "300px")
                               )
                             )
                             )
              ),
    
    
    fluidRow(
        column(width = 6,
          tags$span(HTML("<h4>Relative Probability (Ratio to Reference)</h4>"),
                  infotooltip(HTML("This is the ratio of each species' estimated occupancy probability to the reference occupancy probability.",
				   "For example, if the Superb Parrot has a ratio of '2', then it is estimated that the Superb Parrot is twice as likely to live in your farm's woodland than in the reference farm.",
                                   "The species with the 10 biggest ratios are shown.",
				   "<br><br>",
				   "Each row is a species. The ratio is given in the white box. The length and colours of the bars also represent the ratio.",
				   "Hover over a bar to get more information about that species.",
                                   "<br><br>The reference can be set using the 'Update' button and 'Use default' checkbox below this figure.",
                                   "<br><br>The ratios of all species can be seen by clicking 'View More Detail' or downloading a report."))
          ),
          plotly::plotlyOutput(ns("diff_species"), height = "300px")
        )
        ),
      fluidRow(
        column(width = 6,
          if (isTRUE(getOption("shiny.testmode"))){
            downloadButton(ns("downloaddataverbose"), "Verbose Prediction Data", class = "download_badge")
          },
          actionButton(ns("moredetail"), "View More Detail", class = "download_badge"),
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
                              report_path){
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
        data <- compile_predictions(current_values(), refpredictions())
        session$sendCustomMessage("predictionsmade", "nothing") #for usage tracking
        data
      })
      
      # draw species plots
        # req(data$species_prob_current)
        output$common_species <- plotly::renderPlotly({
          validate(need(datar()$species_prob_current, label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_common(tocommon(datar()$species_prob_current))
        })
        output$diff_species <- plotly::renderPlotly({
          validate(need(datar()$spec_different, label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
          species_plotly_different(datar()$spec_different)
        })

      
      # draw species richness
      output$species_richness <- renderPlot({
        validate(need(datar()$species_richness, ""))
        richness_plot(datar()$species_richness)
      })
      
      # modal more detail stuff
      observeEvent(input$moredetail, {
        moredetailopens(moredetailopens() + 1)
        showModal(
          modalDialog(
            predictionsdetailUI(ns("detail"), isolate(datar()$speciesinfo_topten), isolate(datar()$speciesinfo_botten)),
            title = "More Detail on Predictions",
            size = "l",
      easyClose = TRUE,
            footer = tagList(
              actionButton(ns("hide"), "Hide"),
            )
          )
        )
      })
      
      observeEvent(input$hide, 
                   removeModal()
      )
      
      predictionsdetailServer("detail", datar(), moredetailopens)
      
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
            saveRDS(reactiveValuesToList(datar()), file)
          })
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
      fluidRow(predictionsUI("pred")),
      theme = bslib::bs_theme(version = 5, "lumen"))
      },
           function(input, output, session){
             predictionsServer("pred", 
                               current_values,
                               reactiveVal(species_prob_mean),
                               model_data, 
                               report_path)
           })
}
