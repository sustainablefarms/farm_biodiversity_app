# predictions module
predictionsUI <- function(id, usf){
  ns <- NS(id)
  tagList(
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    # the following enables bootstrap 3's inbuilt popovers
    tags$script("$(function () {
      $('[data-toggle=popover]').popover()
    })"),
      tags$span(HTML("<plottitle>Expected Number of Species</plottitle>"),
                  infotooltip(paste("The middle bar is the expected number of birds species in our model that we predict will be occupying at least one patch on your farm.",
                                    "<br><br>The top bar is the number of species we expect if there is 15 hectares of woody vegetation canopy within 500m of every patch centre.",
                                    "The lower bar is the number of species we expect if there is only 1.5 hectares of woody vegetation canopy within 500m of every patch centre.",
                              "<br><br>Each species was assigned an occupancy probability equal to the maximum of all patches (we use the maximum as we expect occupancy between patches to be highly correlated)."
                              ),
                              `data-container`="body",
                              placement = "bottom")
      ),
      plotOutput(ns("species_richness"), height = "250px"),
      fluidRow(
        column(width = 6, 
           tags$span(HTML("<plottitle>Most Likely Species</plottitle>"),
                  infotooltip(title = HTML("The 10 most likely species to live on your farm according to our model.",
"Bar length and printed percentage indicate the estimated probability of occupancy for each of the species, ignoring interactions between species.",
"When there are multiple patches, the occupancy probability is the maximum of the occupancy probability of the individual patches.",
"<br><br>The error bars summarise uncertainty due to the uncertainty of the model parameters.",
"These error bars are 95&#37; credible intervals (highest posterior density intervals to be precise):",
"if the modelling assumptions are correct and apply to this situation then there is a 95% probability that the actual on-ground occupancy probability is within the credible interval."))
          )
        ),
        column(width = 6,
          tags$span(HTML("<plottitle>Relative to Reference</plottitle>"),
                  infopopover("Ratio of probabilities", "<incomplete - more info to go here>")
        ))
        ),
      plotly::plotlyOutput(ns("common_dif_species"), height = "300px"),
      fluidRow(
        column(width = 4,
            "Reference:",
            actionButton2(ns("savetoreference"), label = "Update", class = "badge_tiny", width = "80px"),
            inlinecheckBoxInput(ns("usesavedreference"), label = "Use saved", value = usf)
               ),
        column(width = 6, offset = 2,
          style = "text-align: right",
          if (isTRUE(getOption("shiny.testmode"))){
            downloadButton(ns("downloaddataverbose"), "Verbose Prediction Data", class = "download_badge")
          },
          actionButton(ns("moredetail"), "View More Detail", class = "download_badge"),
          downloadButton(ns("downloaddata"), "Table", class = "download_badge"),
          downloadButton(ns("downloadreport"), "Report", class = "download_badge")
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
        refXocc = NULL,
        species_prob_current = NULL,
        species_prob_ref = NULL,
        species_richness = NULL,
        toptennames = NULL,
        speciesinfo_topten = NULL,
        speciesinfo_botten = NULL)
      ns <- session$ns
      referencevals <- reactiveVal(value = new_data_mean, label = "Reference Attributes")
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
          if (isTRUE(input$usesavedreference)){
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
                            "More woodland nearby",
                            "Your estimate",
                   "Less woodland nearby"
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
        referencevals(data$Xocc)
        referencepred(data$species_prob_current)
      })
      
      # draw species plots
      observe({
        # req(data$species_prob_current)
        validate(need(data$species_prob_current, label = "")) # could also use req here. Moved outside so that shinytest doesn't when no predictions
        output$common_dif_species <- plotly::renderPlotly({
          species_plotly_both(
            tocommon(data$species_prob_current),
            data$spec_different)
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
      
      if (isTRUE(getOption("shiny.testmode"))){
        output$downloaddataverbose <- downloadHandler(
          filename = "predictions.rds",
          content = function(file) {
            saveRDS(reactiveValuesToList(data), file)
          })
      }
      
      reactive(input$usesavedreference)
    })
}


