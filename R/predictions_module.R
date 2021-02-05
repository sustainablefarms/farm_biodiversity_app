# predictions module
predictionsUI <- function(id){
  ns <- NS(id)
  tagList(
      HTML("<div class='subheader'><h2>BIRD BIODIVERSITY</h2></div>"),
      actionButton(ns("moredetail"), "View More Detail"),
      plotOutput(ns("species_richness"), height = "200px"),
      fluidRow(
        column(width = 6, # first biodiversity plot
          plotOutput(ns("common_species"), height = "300px")
        ),
        column(width = 6,
          plotOutput(ns("different_species"), height = "300px")
        )
      )
  )
}

predictionsServer <- function(id, 
                              selected_region,
                              current_values,
                              model_data,
                              new_data_mean){
  moduleServer(
    id,
    function(input, output, session){
      data <- reactiveValues(
        species_predictions = NULL,
        species_richness = NULL)
      ns <- session$ns
      
      observe({
        if(
          length(selected_region) > 0 &
          length(current_values$woody_veg) == current_values$patches &
          !any(is.na(current_values$woody_veg))
        ){
          # saveRDS(current_values, file = "current_values.rds"); stop("Saving current values - app is in debug mode and will end")
          preddata <- compute_prediction_data(model_data, current_values, new_data_mean)
          data$species_predictions <- preddata$species_predictions
          data$species_richness <- preddata$species_richness
          
        }else{
          data$species_predictions <- NULL
          data$species_richness <- NULL
        }
      })
      
      # draw species plots
      output$common_species <- renderPlot({
        validate(need(data$species_predictions, ""))
        species_ggplot(
          df = data$species_predictions$common,
          title = "Most likely species at any patch",
          add_plus = FALSE,
          errorbar = TRUE)
      })
      output$different_species <- renderPlot({
        validate(need(data$species_predictions, ""))
        species_ggplot(
          df = data$species_predictions$different,
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
          plotOutput(ns("species_probInModal")),
          plotOutput(ns("functdivplot")),
          title = "More Detail on Predictions",
          footer = tagList(
            actionButton(ns("hide"), "Hide"),
          )
        ))
      })
      
      observeEvent(input$hide, 
                   removeModal()
      )
      
      output$species_probInModal <- renderPlot({
        species_ggplotInModal(model_data, current_values, new_data_mean,
                              data$points, selected_region)
      })
      output$functdivplot <- renderPlot({
        functdivplot(model_data, current_values, 
                     data$points, selected_region)[[2]]
      })
    })
}


