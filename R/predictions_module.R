# predictions module
predictionsUI <- function(id, usedflt){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
      tags$p(class = "alignleft",
        HTML("<plottitle>Expected Number of Species</plottitle>"),
                  infotooltip(paste("The <em>second</em> bar is the expected number of birds species in our model that we predict will be occupying at least one patch on your farm.",
                                    "<br><br>",
                                    "The top bar is the number of species we expect if there is only 1.5 hectares of woody vegetation canopy within 500m of every patch centre.",
				    "The third bar is the number of species we expect if there is 15 hectares of woody vegetation canopy within 500m of every patch centre.",
				    "The final bar is the number of species we expect from your reference estimates.",
                              "<br><br>Each species was assigned an occupancy probability equal to the maximum of all patches (we use the maximum as we expect occupancy between patches to be highly correlated)."
                              ),
                              `data-container`="body",
                              placement = "bottom")
      ),
      tags$p(class =  "alignright",
             tags$em(uiOutput(ns("warn"), inline = TRUE))),
      tags$div(style="clear: both;"),
      plotOutput(ns("species_richness"), height = "250px"),
      fluidRow(
        column(width = 6, 
           tags$div(HTML("<plottitle>Most Likely Species</plottitle>"),
                  infotooltip(title = tags$html(tags$p("The 10 most likely species to live in your farm's box gum grassy woodland according to our model."),
					   proboccplotdescription))
          ),
          plotly::plotlyOutput(ns("common_species"), height = "300px")
        ),
        column(width = 6,
          tags$span(HTML("<plottitle>Relative Probability (Ratio to Reference)</plottitle>"),
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
          style = "text-align: right",
            "Reference:",
          infotooltip(title = tags$html(tags$em("Comparing to Other Estimates"),
				  referencesblurb,
                                   tags$p(tags$em("Update"), "sets the reference estimates to the current estimates.",
                                   "After setting the reference estimates for the first time, uncheck", tags$em("Use Default"), "to use them.",
                                   "Initially the ratio of occupancy probabilities to reference probabilities will be all 1.",
                                   "This will be the case until you alter attributes of the farm's woodland."),
                                   tags$p(tags$em("Use Default,"), "when checked, overides the saved estimates with the default estimates.",
                                   "Uncheck", tags$em("Use Default"), "to use the saved reference.")),
		      HTML = TRUE),
            actionButton2(ns("savetoreference"), label = "Update", class = "badge_tiny", width = "80px"),
            inlinecheckBoxInput(ns("usedefaultreference"), label = "Use Default", value = is.null(usedflt) | isTRUE(usedflt))
               )
      )
  )
}

predictionsServer <- function(id, 
                              current_values,
                              model_data,
                              report_path){
  moduleServer(
    id,
    function(input, output, session){
      data <- reactiveValues(
        Xocc = NULL,
        refXocc = new_data_mean,
        species_prob_current = NULL,
        species_prob_ref = NULL,
        species_richness = NULL,
        toptennames = NULL,
        speciesinfo_topten = NULL,
        speciesinfo_botten = NULL)
      ns <- session$ns
      modwmeanXocc <- msod::supplant_new_data(model_data, new_data_mean, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                                                     model_data$XoccProcess$scale,
                                                                                                     model_data$XoccColNames)})
      species_prob_mean <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwmeanXocc)
      referencepred <- reactiveVal(value = species_prob_mean,
                                   label = "Reference Predictions")
      
      observe({
        if(
          current_values()$locationcomplete & current_values()$allpatchcomplete
        ){
          # saveRDS(isolate(current_values()), file = "current_values.rds"); stop("Saving current values - app is in debug mode and will end")
          data$Xocc <- newXocc_fromselected(current_values())
          modwXocc <- msod::supplant_new_data(model_data, data$Xocc, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                               model_data$XoccProcess$scale,
                                                                               model_data$XoccColNames)})
          print(modwXocc$data$Xocc)
          if (!isTRUE(input$usedefaultreference)){
            data$species_prob_ref <- referencepred()
          } else {
            data$species_prob_ref <- species_prob_mean
          }
          data$species_prob_current <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwXocc)
          data$spec_different <- todifferent(data$species_prob_current, data$species_prob_ref)
          species_richness_raw <- rbind(compute_richness(model_data, data$Xocc),
                                         reference = sum(data$species_prob_ref[, "median"]))           # add in reference
          species_richness_raw$category <- factor(1:4, levels = 4:1,
                 labels = c(
                            "Reference estimate",
                            "More woody canopy nearby",
                            "Your estimate",
                   "Less woody canopy nearby"
                   ),
                 ordered = TRUE
          )
          data$species_richness <- species_richness_raw

          topten <- order(data$species_prob_current[, "median"], decreasing = TRUE)[1:10]
          botten <- order(data$species_prob_current[, "median"], decreasing = FALSE)[1:10]
          data$toptennames <- row.names(data$species_prob_current)[topten]
          data$speciesinfo_topten <- speciesinfo[row.names(data$species_prob_current)[topten], ]
          data$speciesinfo_botten <- speciesinfo[row.names(data$species_prob_current)[botten], ]
          # saveRDS(isolate(reactiveValuesToList(data)), file = "data.rds"); stop("Saving data - app will end now")
        } else {
          # data <- lapply(data, function(x) NULL)
          data$Xocc <- NULL
          data$species_prob_current <- NULL
          data$species_prob_ref <- NULL
          data$species_richness <- NULL
        }
      })
      
      # reference prediction saving
      observeEvent(input$savetoreference, {
        data$refXocc <- data$Xocc
        referencepred(data$species_prob_current)
      })
      
      # draw species plots
      observe({
        # req(data$species_prob_current)
        validate(need(data$species_prob_current, label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
        output$common_species <- plotly::renderPlotly({
          species_plotly_common(tocommon(data$species_prob_current))
        })
        output$diff_species <- plotly::renderPlotly({
          species_plotly_different(data$spec_different)
        })
      })

      
      # draw species richness
      output$species_richness <- renderPlot({
        validate(need(data$species_richness, ""))
        richness_plot(data$species_richness)
      })
      
      # modal more detail stuff
      observeEvent(input$moredetail, {
        showModal(
          modalDialog(
            predictionsdetailUI(ns("detail"), isolate(data$speciesinfo_topten), isolate(data$speciesinfo_botten)),
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
      
      predictionsdetailServer("detail", data)
      
      output$downloaddata <- downloadHandler(
        filename = "predictions.csv",
        content = function(file) {
          outdata <- data$species_prob_current
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
            saveRDS(reactiveValuesToList(data), file)
          })
      }
      
      reactive(input$usedefaultreference)
    })
}


