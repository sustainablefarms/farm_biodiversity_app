# predictions module
predictionsUI <- function(id){
  ns <- NS(id)
  tagList(
      HTML("<div class='subheader'><h2>BIRD BIODIVERSITY</h2></div>"),
      uiOutput(ns("species_richness_title")),
      plotOutput(ns("species_richness"), height = "200px"),
      fluidRow(
        column(width = 12, # first biodiversity plot
          plotly::plotlyOutput(ns("common_dif_species"), height = "300px")
        )
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
        species_richness = NULL)
      ns <- session$ns
      
      observe({
        if(
          length(current_values$AnnPrec) > 0 & #this is here because for some reason selected_region doesn't work
          length(current_values$woody_veg) == current_values$patches &
          !any(is.na(current_values$woody_veg))
        ){
          # saveRDS(isolate(reactiveValuesToList(current_values)), file = "current_values.rds"); stop("Saving current values - app is in debug mode and will end")
          data$Xocc <- newXocc_fromselected(current_values) 
          data$species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                                    data$Xocc)
          data$species_prob_ref <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
            new_data_mean)
          data$spec_different <- todifferent(data$species_prob_current, data$species_prob_ref)
          data$species_richness <- compute_richness(model_data, data$Xocc)
        }else{
          # data <- lapply(data, function(x) NULL)
          data$Xocc <- NULL
          data$species_prob_current <- NULL
          data$species_prob_ref <- NULL
          data$species_richness <- NULL
        }
      })
      
      
      output$species_richness_title <- renderUI({
        validate(need(data$species_prob_current, ""))
        list(
          actionButton(ns("moredetail"), "View More Detail"),
          downloadButton(ns("downloaddata"), "Download to .csv"),
          downloadButton(ns("downloadreport"), "Download to Report")
        )
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
            predictionsdetailUI(ns("detail")),
            # plotly::plotlyOutput(ns("detail-species_InModal"), height = "800px")
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
                            output_file = file,
                            envir = new.env(parent = environment())
          )
        }
      )
    })
}


