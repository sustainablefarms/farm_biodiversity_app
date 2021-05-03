# predictions module
predictionsUI <- function(id){
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
      tags$span(HTML("<plottitle>Expected Number of Common Spring Bird Species</plottitle>"),
                  infopopover("Expected Number of Species in our Model",
                              paste("This is the expected number of birds species in our model that we predict will be living in at least one patch on your farm.",
                              "The occupancy probability of each species was considered the highest occupancy probability of all provided patches.",
                              "The estimate uses the median of latent variable values, so interactions between species are incorporated.",
                              "Patch occupancy is considered to be highly correlated between patches, hence the use of the maximum across all patches"),
                              trigger = "click")
      ),
      plotOutput(ns("species_richness"), height = "200px"),
      fluidRow(
        column(width = 6, 
           tags$span(HTML("<plottitle>Most Likely Species</plottitle>"),
                  infopopover("Most Likely Species", "<incomplete - more info to go here>"))
        ),
        column(width = 6,
          tags$span(HTML("<plottitle>Relative to an Average Patch</plottitle>"),
                  infopopover("Ratio of probabilities", "<incomplete - more info to go here>")
        ))
        ),
      plotly::plotlyOutput(ns("common_dif_species"), height = "300px"),
      tags$div(
          style = "text-align: right",
          actionButton(ns("moredetail"), "View More Detail", class = "download_badge"),
          downloadButton(ns("downloaddata"), "Download Predictions", class = "download_badge"),
          downloadButton(ns("downloadreport"), "Download Report", class = "download_badge")
        )
  )
}

predictionsServer <- function(id, 
                              current_values,
                              model_data,
                              new_data_mean,
                              report_path){
  moduleServer(
    id,
    function(input, output, session){
      data <- reactiveValues(
        Xocc = NULL,
        species_prob_current = NULL,
        species_prob_ref = NULL,
        species_richness = NULL,
        toptennames = NULL,
        speciesinfo_topten = NULL,
        speciesinfo_botten = NULL)
      ns <- session$ns
      
      observe({
        if(
          current_values()$locationcomplete & current_values()$allpatchcomplete
        ){
          # saveRDS(isolate(current_values()), file = "current_values.rds"); stop("Saving current values - app is in debug mode and will end")
          data$Xocc <- newXocc_fromselected(current_values())
          modwXocc <- msod::supplant_new_data(model_data, data$Xocc, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                               model_data$XoccProcess$scale,
                                                                               model_data$XoccColNames)})
          modwmeanXocc <- msod::supplant_new_data(model_data, new_data_mean, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                               model_data$XoccProcess$scale,
                                                                               model_data$XoccColNames)})
          data$species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(modwXocc)
          data$species_prob_ref <- msod::poccupancy_mostfavourablesite.jsodm_lv(modwmeanXocc)
          data$spec_different <- todifferent(data$species_prob_current, data$species_prob_ref)
          data$species_richness <- compute_richness(model_data, data$Xocc)
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
    })
}


