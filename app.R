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

# load requisite packages
library(sf)
library(shiny)
library(shinythemes)
library(plotly)
library(Rfast)
library(ggbeeswarm)
library(ggplot2)
library(shinyBS)

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
    HTML("<div class='subheader'><h2>REGION</h2></div>"),
    selectInput(
      inputId = "spatial_type",
      label = NULL,
      choices = list(
        "ABS SA2 regions" = "abs_sa2",
        "Federal Electorates" = "electorates_federal"),
      width = "100%"),
    plotlyOutput("plot_points", height = "200px"),
    plotOutput("map", height = "200px"),
    # HTML("<div class='subheader'><h2>CLIMATE</h2></div>"),
    HTML("<br>"),
    column(width = 6,
      uiOutput("show_tempmean"),
      uiOutput("show_temprange")
    ),
    column(width = 6,
      uiOutput("show_precip"),
      uiOutput("show_precipwarmq")
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
        bsPopover("patch_number_1", 
                  "<h2>A patch is...<h2>",
                  "a region of woody vegetation that is (1) 1ha - 10ha (to check) in area, (2) has similar vegetation structure throughout and (3) is approximately 50m from other woody vegetation.",
                  ),
        div(id = "placeholder")
      )
    ),
    fluidRow(
      HTML("<div class='subheader'><h2>BIODIVERSITY</h2></div>"),
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
server <- function(input, output) {

  # set up required data
  # 1. from model
  model_data <- readRDS("data/model_data.rds")
  new_data_mean <- as.data.frame(
    matrix(data = model_data$XoccProcess$center, nrow = 1, ncol = 11))
  colnames(new_data_mean) <- names(model_data$XoccProcess$center)
  new_data_mean <- new_data_mean[, c(2:10)]
  new_data_mean$NMdetected[1] <- 1

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
    AnnMeanTemp = NULL,
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
    ), "A patch is a region of woody vegetation that is (1) 1ha - 10ha (to check) in area, (2) has similar vegetation structure throughout and (3) is approximately 50m from other woody vegetation.")
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
  # choose what spatial data to use
  observeEvent(input$spatial_type, {
    if(input$spatial_type != "none"){
      data$points <- readRDS("data/sa2_points.rds")
    }
  })

  # draw a scatterplot of the centroids of selected zones
  output$plot_points <- renderPlotly({
    validate(
      need(data$points, "")
    )
    plot_ly(
      data$points,
      x = ~longitude,
      y = ~latitude,
      type = "scatter",
      mode = "markers",
      source = "region_map",
      marker = list(
        size = 10,
        color = ~color
      ),
      hoverinfo = "text",
      text = ~label
    ) %>% layout(
      xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
      yaxis = list(scaleanchor = "x",
        title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
      margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
      paper_bgcolor='transparent',
      plot_bgcolor = 'transparent'
    ) %>%
    config(displayModeBar = FALSE)
    # event_register(p, 'plotly_click')
  })

  # observe clicks on the region plot
  observe({
    if(!is.null(data$points)){
      click_region <- event_data(
        event = "plotly_click",
        source = "region_map"
      )$pointNumber + 1 # Note: plotly uses Python-style indexing, hence +1
      data$selected_region <- data$points$label[click_region]
      # add climate data
      climate_row <- which(data$points$label == data$selected_region)
      current_values$AnnMeanTemp <- data$points$AnnMeanTemp[climate_row]
      current_values$AnnTempRange <- data$points$AnnTempRange[climate_row]
      current_values$AnnPrec <- data$points$AnnPrec[climate_row]
      current_values$PrecSeasonality <- data$points$PrecSeasonality[climate_row]
      current_values$PrecWarmQ <- data$points$PrecWarmQ[climate_row]
    }
  })

  # draw a map
  output$map <- renderPlot({
    validate(need(data$selected_region, ""))
    data$polygons <- readRDS("data/sa2_polygons.rds")
    map_text <- data$points[data$points$label == data$selected_region, ]
    map_text$label <- paste(strsplit(map_text$label, " ")[[1]], collapse = "\n")
    ggplot(data$polygons[data$polygons$SA2_NAME16 == data$selected_region, ]) +
      geom_sf(fill = "grey90", color = "grey30") +
      geom_text(data = map_text,
        mapping = aes(x = longitude, y = latitude, label = label),
        color = "grey30",
        alpha = 0.5,
        size = 5
      ) +
      theme_void()
  })

  ## CLIMATE
  output$show_tempmean <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_tempmean_modal",
        label = HTML(paste0(
          "Annual<br>Mean<br>Temperature<h3>",
          round(current_values$AnnMeanTemp * 0.1, 1),
          "&deg;C</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_temprange <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_temprange_modal",
        label = HTML(paste0(
          "Annual<br>Temperature<br>Range<h3>",
          format(current_values$AnnTempRange * 0.1, digits = 3, trim = TRUE),
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
          "ml</h3>")),
        class = "badge",
        width = "100%"
      )
    }
  })

  output$show_precipwarmq <- renderUI({
    if(length(data$selected_region) > 0){
      actionButton2(
        inputId = "show_precipwarmq_modal",
        label = HTML(paste0(
          "Precipitation<br>Warmest<br>Quarter<h3>",
          current_values$PrecWarmQ,
          "ml</h3>")),
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
  observeEvent(input$show_tempmean_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "AnnMeanTemp"
    click_values$climate_title <- "Mean annual temperature (Celsius)"
    climate_modal()
  })
  observeEvent(input$show_temprange_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "AnnTempRange"
    click_values$climate_title <- "Annual temperature range (Celsius)"
    climate_modal()
  })
  observeEvent(input$show_precip_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "AnnPrec"
    click_values$climate_title <- "Annual precipitation (ml)"
    climate_modal()
  })
  observeEvent(input$show_precipwarmq_modal, {
    validate(need(data$selected_region, ""))
    click_values$climate <- "PrecWarmQ"
    click_values$climate_title <- "Precipitation of the warmest quarter (ml)"
    climate_modal()
  })



  ## BIODIVERSITY
  observe({
    if(
      length(data$selected_region) > 0 &
      length(current_values$woody_veg) == current_values$patches &
      !any(is.na(current_values$woody_veg))
    ){
      new_data <- data.frame(
        AnnMeanTemp = data$points$AnnMeanTemp[data$points$label == data$selected_region],
        AnnPrec = data$points$AnnPrec[data$points$label == data$selected_region],
        AnnTempRange = data$points$AnnTempRange[data$points$label == data$selected_region],
        PrecSeasonality = data$points$PrecSeasonality[data$points$label == data$selected_region],
        PrecWarmQ = data$points$PrecWarmQ[data$points$label == data$selected_region],
        SurveyYear = current_values$year,
        woody500m = current_values$woody_veg,
        ms = current_values$midstorey,
        NMdetected = current_values$noisy_miner
      )
      species_prediction_df <- data.frame(
        species = rownames(model_data$u.b),
        prediction_current = as.numeric(poccupancy_standalone_nolv(
          new_data,
          model_data$XoccProcess,
          model_data$u.b)),
        prediction_mean = as.numeric(poccupancy_standalone_nolv(
          new_data_mean,
          model_data$XoccProcess,
          model_data$u.b)))
      species_prediction_df$difference <- species_prediction_df$prediction_current -
        species_prediction_df$prediction_mean

      # get dataset of top 10 most common species
      sp_current <- species_prediction_df[
        order(species_prediction_df$prediction_current, decreasing = TRUE)[1:10], c(1:2)]
      colnames(sp_current)[2] <- "value"

      # ditto for 'most different' species
      sp_different <- species_prediction_df[
        order(species_prediction_df$difference, decreasing = TRUE)[1:10], c(1, 4)]
      colnames(sp_different)[2] <- "value"

      data$species_predictions <- list(
        common = sp_current,
        different = sp_different)

      # richness calculations
      # get richness
      richness_data <- list(new_data_mean, new_data, new_data_mean)
      richness_data[[1]]$ms <- 0; richness_data[[3]]$ms <- 10
      richness_data[[1]] <- richness_data[[1]][rep(1, nrow(new_data)), ]
      richness_data[[1]]$woody500m <- 2; richness_data[[3]]$woody500m <- 20
      richness_data[[3]] <- richness_data[[3]][rep(1, nrow(new_data)), ]
      richness_predictions <- lapply(richness_data, function(a){
        multisiterichness_nolv(a, model_data$XoccProcess, model_data$u.b)
      })
      richness_df <- as.data.frame(do.call(rbind, richness_predictions))
      richness_df$category <- factor(seq_len(3), levels = seq_len(3),
        labels = c("Less vegetation", "Your estimate", "More vegetation"))

      data$species_richness <- richness_df

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
      title = "Most likely species",
      add_plus = FALSE)
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
    ggplot(data$species_richness, aes(x = category, y = Erichness, fill = category)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(
        y = c(0, max(data$species_richness$Erichness + data$species_richness$Vrichness) + 3)) +
      scale_x_discrete(position = "top") +
      scale_discrete_manual(aesthetics = "fill", values = c("#81a2b3", "#4e839c", "#81a2b3")) +
      geom_errorbar(aes(ymin = Erichness - Vrichness, ymax = Erichness + Vrichness), width = 0.2) +
      coord_flip() +
      ggtitle("Number of bird species") +
      theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "grey90", colour = NA),
        panel.border = element_blank()
      )
  })


} # end server

shinyApp(ui, server)