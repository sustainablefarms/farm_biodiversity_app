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

# UI
ui <- fluidPage(
  includeCSS("./www/base.css"),
  fluidRow(HTML("
    <div class='header'>
      <img class='logo' src='SF Logo Vertical - Transparent Background.png' alt='SF logo'>
      <span class='main'>BGGW Bird Biodiversity Estimator</span>
      <span class='subtitle'>by Martin Westgate & Kassel Hingee</span>
    </div>
  ")),
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
    HTML("<div class='subheader'><h2>CLIMATE</h2></div>"),
    plotOutput("climate", height = "300px")
  ),
  column(width = 1),
  column(width = 6,
    fluidRow(
      HTML("<div class='subheader'><h2>FARM</h2></div>"),
      column(width = 3,
        sliderInput(
          inputId = "n_patches",
          label = "Number of vegetation patches",
          min = 1, max = 6, step = 1, value = 1)),
      column(width = 3,
        sliderInput(
          inputId = "pc_woody_veg",
          label = "Amount of woody vegetation (%)",
          min = 2, max = 20, step = 2, value = 8)),
      column(width = 3,
        sliderInput(
          inputId = "pc_midstorey",
          label = "Amount of shrub cover (%)",
          min = 0, max = 10, step = 1, value = 6)),
      column(width = 3,
        sliderInput(
          inputId = "year",
          label = HTML("<br>Year"), # for consistency in arrangement with other labels
          sep = "",
          min = 2000, max = 2018, step = 2, value = 2018))
    ),
    fluidRow(
      column(width = 8),
      column(width = 4,
        checkboxInput(
          inputId = "noisy_miner",
          label = "Noisy Miners present?",
          value = TRUE)
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
    species_predictions = NULL
  )

  ## REGION
  # choose what spatial data to use
  observeEvent(input$spatial_type, {
    if(input$spatial_type != "none"){
      data$points <- readRDS("data/sa2_points.rds")
      data$climate <- data.frame(
        label = rep(data$points$label, 4),
        variable = rep(
          c("Annual Mean Temperature", "Annual Preciptiation",
            "Annual Temperature Range", "Precipitation Seasonality"),
          each = nrow(data$points)),
        value = c(data$points$AnnMeanTemp * 0.1, data$points$AnnPrec,
          data$points$AnnTempRange * 0.1, data$points$PrecSeasonality))
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

  output$climate <- renderPlot({
    validate(need(data$selected_region, ""))
    ggplot(data$climate, aes(x = value, y = 1, color = variable)) +
      facet_wrap(vars(variable), ncol = 2, scales = "free_x") +
      geom_quasirandom(
        data = data$climate[data$climate$label != data$selected_region, ],
        size = 2, groupOnX = FALSE) +
      geom_quasirandom(
        data = data$climate[data$climate$label == data$selected_region, ],
        size = 4, groupOnX = FALSE, color = "black") +
      theme_bw() +
      scale_color_manual(values = c("#4e839c", "#4e9c63", "#81a2b3", "#82b38f")) +
      theme(
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 10),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "grey90", colour = NA),
        panel.border = element_blank()
      )
  })

  ## BIODIVERSITY
  observeEvent({
    # input$n_patches # not implemented yet
    input$pc_woody_veg
    input$pc_midstorey
    input$year
    input$noisy_miner
    data$selected_region
  }, {
    if(length(data$selected_region) > 0){
      new_data <- data.frame(
        AnnMeanTemp = data$points$AnnMeanTemp[data$points$label == data$selected_region],
        AnnPrec = data$points$AnnPrec[data$points$label == data$selected_region],
        AnnTempRange = data$points$AnnTempRange[data$points$label == data$selected_region],
        PrecSeasonality = data$points$PrecSeasonality[data$points$label == data$selected_region],
        SurveyYear = input$year,
        PrecWarmQ = data$points$PrecWarmQ[data$points$label == data$selected_region],
        woody500m = input$pc_woody_veg,
        ms = input$pc_midstorey,
        NMdetected = as.numeric(input$noisy_miner)
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
      richness_data <- list(new_data, new_data, new_data)
      richness_data[[1]]$ms <- 0; richness_data[[3]]$ms <- 10
      richness_data[[1]]$woody500m <- 2; richness_data[[3]]$woody500m <- 20
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
      title = "    Most common species",
      add_plus = FALSE)
  })
  output$different_species <- renderPlot({
    validate(need(data$species_predictions, ""))
    species_ggplot(
      df = data$species_predictions$different,
      title = "    Locally prevalent species",
      add_plus = TRUE)
  })

  # draw species richness
  output$species_richness <- renderPlot({
    validate(need(data$species_richness, ""))
    ggplot(data$species_richness, aes(x = category, y = Erichness, fill = category)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(expand = c(0, 0)) +
      expand_limits(y = c(0, max(richness_df$Erichness + richness_df$Vrichness) + 3)) +
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