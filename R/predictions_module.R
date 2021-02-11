# predictions module
predictionsUI <- function(id){
  ns <- NS(id)
  tagList(
      HTML("<div class='subheader'><h2>BIRD BIODIVERSITY</h2></div>"),
      uiOutput(ns("species_richness_title")),
      plotOutput(ns("species_richness"), height = "200px"),
      fluidRow(
        column(width = 6, # first biodiversity plot
          plotlyOutput(ns("common_species"), height = "300px")
        ),
        column(width = 6,
          plotOutput(ns("different_species"), height = "300px")
        )
      )
  )
}

predictionsServer <- function(id, 
                              current_values,
                              model_data,
                              new_data_mean){
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
          # saveRDS(current_values, file = "current_values.rds"); stop("Saving current values - app is in debug mode and will end")
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
        actionButton(ns("moredetail"), "View More Detail")
      })
      
      # draw species plots
      output$common_species <- renderPlotly({
        validate(need(data$species_prob_current, ""))
        species_plotly(
          df = tocommon(data$species_prob_current),
          title = "Most likely species at any patch",
          add_plus = FALSE,
          errorbar = TRUE)
      })
      output$different_species <- renderPlot({
        validate(need(data$spec_different, ""))
        species_ggplot(
          df = data$spec_different,
          title = "Locally prevalent species",
          add_plus = TRUE)
      })
      
      # draw species richness
      output$species_richness <- renderPlot({
        validate(need(data$species_richness, ""))
        richness_plot(data$species_richness)
      })
      
      # modal more detail stuff
      observeEvent(input$moredetail, {
        showModal(modalDialog(
          plotOutput(ns("species_probInModal"), height = "800px"),
          title = "More Detail on Predictions",
          size = "l",
          footer = tagList(
            actionButton(ns("hide"), "Hide"),
          )
        ))
      })
      
      observeEvent(input$hide, 
                   removeModal()
      )
      
      output$species_probInModal <- renderPlot({
        species_ggplotInModal(model_data, current_values, new_data_mean)
      })
    })
}


