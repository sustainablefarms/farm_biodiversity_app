# shiny app to interact with model outcomes from the linking data project
  # select a region
    # this gives climate data
  # set some parameters about the farm
    # presence of Noisy Miners
    # % woody veg
    # % midstorey cover
    # number of patches (?)
  # get prediction of
    # likely species (list)
    # richness (estimate)
    # what you could do to improve this

# This page is quite informative on using custom package dependencies: https://github.com/rstudio/rsconnect/issues/88
# rsconnect::appDependencies()
# rsconnect::deployApp(appName = "birdbio_dev3")
# rsconnect::terminateApp("birdbio_dev2")
# rsconnect::purgeApp("birdbio_dev2")


# load requisite packages
# devtools::install_github("https://github.com/sustainablefarms/msod")
library(msod)
library(sf)
library(shiny)
library(shinythemes)
library(plotly)
library(Rfast)
library(ggbeeswarm)
library(ggplot2)
library(shinyBS)
library(tippy)

# UI
ui <- fluidPage(
  includeCSS("./www/base.css"),
  # fluidRow(HTML("
  #   <div class='header'>
  #     <img class='logo' src='SF Logo Vertical - Transparent Background.png' alt='SF logo'>
  #     <span class='main'>DRAFT Woodland Remnant Bird Biodiversity Estimator</span>
  #     <span class='subtitle'><br>Version 0.1. By Martin Westgate & Kassel Hingee</span>
  #   </div>
  # ")),
  HTML("<div class='header'>"),
  fluidRow(
    column(width = 2,
    HTML("
      <img class='logo' src='SF Logo Vertical - Transparent Background.png' alt='SF logo'>
  ")),
  column(width = 10, offset = 0, HTML("
    <span class='main'>Woodland Remnant Bird Biodiversity Estimator</span>
    <span class='subtitle'><br>Version 0.1 (DRAFT). By Martin Westgate & Kassel Hingee</span>
  "))),
  HTML("</div>"),
  column(width = 1),
  column(width = 3,
    selectlocationUI("location"),
    column(width = 6,
      uiOutput("show_maxtemp"),
      uiOutput("show_mintemp")
    ),
    column(width = 6,
      uiOutput("show_precip"),
      uiOutput("show_precseason")
    )
    # plotOutput("climate", height = "300px")
  ),
  column(width = 1),
  column(width = 6,
    fluidRow(
      HTML("<div class='subheader'><h2>FARM</h2></div>"),
      verbatimTextOutput("text"),
      column(width = 3,
        uiOutput("patch_selector"),
        uiOutput("year_selector")
      ),
      column(width = 1),
      column(width = 8,
        actionButton2(
          inputId = "patch_number_1",
          label = "Patch #1",
          class = "patch_badge"),
        tippy::tippy_this("patch_number_1", 
                  paste('<div style="text-align: left">A patch is a region of woodland vegetation that is',
                  '<br>(1) 1ha - 10ha (TBC) in area, ',
                  '<br>(2) has similar vegetation structure throughout, and',
                  '<br>(3) is approximately 50m from other woody vegetation.',
                  '<br><br> Currently the patch must be remnant box gum grassy woodlands.</div>'),
                  arrow = TRUE,
                  interactive = TRUE,
                  options = list(html = "true",
                                 arrow = "roundArrow")),
        div(id = "placeholder")
      )
    ),
    fluidRow(
      HTML("<div class='subheader'><h2>BIRD BIODIVERSITY</h2></div>"),
      actionButton("moredetail", "View More Detail"),
      plotOutput("species_richness", height = "200px"),
      fluidRow(
        column(width = 6, # first biodiversity plot
          plotOutput("common_species", height = "300px")
        ),
        column(width = 6,
          plotOutput("different_species", height = "300px")
        )
      )
    )
  ),
  title = "SF Model Visualiser",
  theme = shinytheme("lumen")
)

# SERVER
server <- function(input, output, session) {

  # set up required data
  # 1. from model
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)

  # 2. reactive values
  data <- reactiveValues(
    points = NULL,
    climate = NULL,
    polygons = NULL,
    selected_region = c(),
    species_predictions = NULL)
  current_values <- reactiveValues(
    patches = 1,
    woody_veg = NA,
    midstorey = NA,
    noisy_miner = NA,
    year = 2018,
    AnnPrec = NULL,
    MaxTWarmMonth = NULL,
    MinTColdMonth = NULL,
    PrecSeasonality = NULL,
    latitude = NULL,
    AnnPrec = NULL,
    AnnTempRange = NULL,
    PrecSeasonality = NULL,
    PrecWarmQ = NULL)
  previous_values <- reactiveValues(
    patches = 1,
    patch_buttons = c(0),
    selected_patch = NULL)
  update <- reactiveValues(
    add_logical = FALSE,
    add_values = NULL,
    remove_logical = FALSE,
    remove_values = NULL)
  click_values <- reactiveValues(
    patches = NULL,
    climate = NULL,
    climate_title = NULL)

  ## FARM

  # number of patches
  output$patch_selector <- renderUI({
    shinyBS::tipify(actionButton2(
      inputId = "choose_n_patches",
      label = HTML(paste0("Number of<br>patches<br><h3>", current_values$patches, "</h3>")),
      class = "badge"
    ), 
    paste('<div style="text-align: left">A patch is a region of woodland vegetation that <br>(1) is 1ha - 10ha <em>(TBC)</em> in area, <br>(2) has similar vegetation structure throughout, and <br>(3) is approximately 50m from other woody vegetation.',
    '<br><br> Currently the patch must be remnant box gum grassy woodlands.</div>'),
    options = list(html = TRUE)
    )
  })
  observeEvent(input$choose_n_patches, {
    showModal(
      modalDialog(
        sliderInput(
          inputId = "n_patches",
          label = "Number of vegetation patches",
          min = 1, max = 6, step = 1, value = current_values$patches),
        actionButton("choose_n_patches_execute", "Save"),
        modalButton("Cancel"),
        title = "Select number of patches",
        footer = NULL,
        easyClose = FALSE
      )
    )
  })

  # once number of patches is chosen, decide whether to add or subtract 'patch' buttons
  observeEvent(input$choose_n_patches_execute, {
    current_values$patches <- input$n_patches
    if(previous_values$patches > current_values$patches){
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- TRUE
      update$remove_values <- seq_len(previous_values$patches)[-seq_len(current_values$patches)]
    }
    if(previous_values$patches == current_values$patches){
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- FALSE
      update$remove_values <- NULL
    }
    if(previous_values$patches < current_values$patches){
      update$add_logical <- TRUE
      update$add_values <- seq_len(current_values$patches)[-seq_len(previous_values$patches)]
      update$remove_logical <- FALSE
      update$remove_values <- NULL
    }
    removeModal()
  })

  # add 'patch' buttons
  observeEvent(update$add_values, {
    if(!is.null(input$n_patches) & update$add_logical){
      lapply(update$add_values, function(a){
        add_reference_ui(
          entry_number = a,
          ui_selector = "placeholder"
        )
      })
      update$add_logical <- FALSE
      previous_values$patches <- current_values$patches
      current_values$woody_veg[update$add_values] <- NA
      current_values$midstorey[update$add_values] <- NA
      current_values$noisy_miner[update$add_values] <- NA
    }
  })

  # substract 'patch' buttons
  observeEvent(update$remove_values, {
    if(!is.null(input$n_patches) & update$remove_logical){
      lapply(update$remove_values, function(a){
        removeUI(
          selector = paste0("#patch_number_", a)
        )
      })
      update$remove_logical <- FALSE
      previous_values$patches <- current_values$patches
      current_values$woody_veg <- current_values$woody_veg[-update$remove_values]
      current_values$midstorey <- current_values$midstorey[-update$remove_values]
      current_values$noisy_miner <- current_values$noisy_miner[-update$remove_values]
    }
  })

  # prediction year
  output$year_selector <- renderUI({
    actionButton2(
      inputId = "choose_prediction_year",
      label = current_values$year, #HTML(paste0("<h3>", current_values$year, "</h3>")),
      class = "badge_small"
    )
  })
  observeEvent(input$choose_prediction_year, {
    showModal(
      modalDialog(
        selectInput(
          inputId = "choose_year",
          label = "Prediction Year",
          choices = seq(2001, 2018, 1),
          selected = current_values$year,
          width = "100%"
        ),
        actionButton("choose_prediction_year_execute", "Save"),
        modalButton("Cancel"),
        title = "Select prediction year",
        footer = NULL,
        easyClose = FALSE
      )
    )
  })
  observeEvent(input$choose_prediction_year_execute, {
    current_values$year <- as.numeric(input$choose_year)
    removeModal()
  })

  # output$text <- renderPrint({
  #   # paste(
  #   #   paste(current_values$mistorey, collapse = "; "),
  #      paste(current_values$woody_veg, collapse = "; ")
  #   #   paste(current_values$noisy_miner, collapse = "; "),
  #   # collapse = "  |   ")
  # })
  # output$text <- renderPrint({previous_values$selected_patch})
  # output$text <- renderPrint({paste(current_values$woody_veg, collapse = "; ")})
  # output$text <- renderPrint({str(click_values$patches)})

  # for each patch, launch a modal to set new values
  observe({
    click_values$patches <- input_tracker(
      input = input,
      string = "patch_number_[[:digit:]]+"
    )
    if(nrow(click_values$patches) == length(previous_values$patch_buttons)){
      update_check <- (click_values$patches$value > previous_values$patch_buttons)
      if(any(update_check)){
        previous_values$selected_patch <- click_values$patches$id[which(update_check)]
        patch_modal(
          value = previous_values$selected_patch,
          woody_veg = current_values$woody_veg[previous_values$selected_patch],
          midstorey = current_values$midstorey[previous_values$selected_patch],
          noisy_miner = current_values$noisy_miner[previous_values$selected_patch]
        )
      }
    }
    previous_values$patch_buttons <- click_values$patches$value
  })

  # collect input values from modal
  observeEvent(input$choose_patch_attributes_execute, {
    current_values$woody_veg[previous_values$selected_patch] <- input[[
      paste0("pc_woody_veg_", previous_values$selected_patch)]]
    current_values$midstorey[previous_values$selected_patch] <- input[[
      paste0("pc_midstorey_", previous_values$selected_patch)]]
    current_values$noisy_miner[previous_values$selected_patch] <- input[[
      paste0("noisy_miner_", previous_values$selected_patch)]]
    removeModal()
  })

  ## REGION
  outOfModule <- selectlocationServer("location")
  observe({
    data$selected_region <- outOfModule$selected_region
    current_values$AnnPrec <- outOfModule$AnnPrec
    data$points <- outOfModule$points
    current_values$MaxTWarmMonth                   <- outOfModule$MaxTWarmMonth
    current_values$MinTColdMonth                   <- outOfModule$MinTColdMonth
    current_values$PrecSeasonality                   <- outOfModule$PrecSeasonality
    current_values$latitude  <- outOfModule$latitude
    current_values$AnnTempRange  <- outOfModule$AnnTempRange
    current_values$PrecSeasonality                   <- outOfModule$PrecSeasonality
    current_values$PrecWarmQ                   <- outOfModule$PrecWarmQ
  })


  ## CLIMATE
  output$show_maxtemp <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_maxtemp_modal",
        label = HTML(paste0(
          "Maximum<br>Temperature<h3>",
          round(current_values$MaxTColdMonth * 0.1, 1),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_mintemp <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_mintemp_modal",
        label = HTML(paste0(
          "Minimum<br>Temperature<h3>",
          format(current_values$MinTColdMonth * 0.1, digits = 3, trim = TRUE),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precip <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_precip_modal",
        label = HTML(paste0(
          "Annual<br>Preciptiation<h3>",
          current_values$AnnPrec,
          "mm</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precseason <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_precseason_modal",
        label = HTML(paste0(
          "Precipitation<br>Seasonality<h3>",
          current_values$PrecSeasonality,
          "</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$climate_plot <- renderPlot({
    if(!is.null(click_values$climate)){
      climate_plot(
        data = data$points,
        variable = click_values$climate,
        region = data$selected_region,
        title = click_values$climate_title)
    }
  })
  # run a different modal for each climate variable
  observeEvent(input$show_maxtemp_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "MaxTWarmMonth"
    click_values$climate_title <- "Maximum temperature (Celsius)"
    climate_modal()
  })
  observeEvent(input$show_mintemp_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "MinTColdMonth"
    click_values$climate_title <- "Minimum temperature (Celsius)"
    climate_modal()
  })
  observeEvent(input$show_precip_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "AnnPrec"
    click_values$climate_title <- "Annual precipitation (mm)"
    climate_modal()
  })
  observeEvent(input$show_precseason_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "PrecSeasonality"
    click_values$climate_title <- "Precipitation seasonality"
    climate_modal()
  })



  ## BIODIVERSITY
  observe({
    if(
      length(data$selected_region) > 0 &
      length(current_values$woody_veg) == current_values$patches &
      !any(is.na(current_values$woody_veg))
    ){
      preddata <- compute_prediction_data(model_data, current_values, new_data_mean,
                                          data$points, data$selected_region)
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
        plotOutput("species_probInModal"),
        plotOutput("functdivplot"),
        title = "More Detail on Predictions",
        footer = tagList(
          actionButton("hide", "Hide"),
        )
      ))
    })
    
    observeEvent(input$hide, 
                 removeModal()
  )
    
  output$species_probInModal <- renderPlot({
      species_ggplotInModal(model_data, current_values, new_data_mean,
                            data$points, data$selected_region)
    })
  output$functdivplot <- renderPlot({
    functdivplot(model_data, current_values, 
                          data$points, data$selected_region)[[2]]
  })

} # end server

shinyApp(ui, server)
