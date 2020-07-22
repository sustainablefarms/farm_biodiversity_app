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
library(shiny)
library(shinythemes)
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
      HTML("<i>Richness plot goes here</i>"),
      fluidRow(
        column(width = 6, # first biodiversity plot
          plotOutput("common_species", height = "300px")
        ),
        column(width = 6,
          HTML("<i>Second species plot goes here</i>")
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
  model_data <- readRDS("data/model_data.rds")
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
    data$points
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
      # data$species_predictions <- new_data
      species_prediction_matrix <- poccupancy_standalone_nolv(
        new_data,
        model_data$XoccProcess,
        model_data$u.b)
      species_prediction_df <- data.frame(
        species = colnames(species_prediction_matrix),
        prediction_current = as.numeric(species_prediction_matrix))
      species_prediction_df <- species_prediction_df[
        order(species_prediction_df$prediction_current, decreasing = TRUE), ]
      data$species_predictions <- species_prediction_df
    }else{
      data$species_predictions <- NULL
    }
  })

  # draw common species plot
  output$common_species <- renderPlot({
    validate(need(data$species_predictions, ""))
    species_prediction_df_small <- data$species_predictions[1:10, ]
    species_prediction_df_small$species <- factor(
      seq_len(10),
      levels = seq_len(10),
      labels = species_prediction_df_small$species)
    ggplot(species_prediction_df_small,
      aes(x = species, y = prediction_current, fill = prediction_current)) +
      geom_bar(stat = "identity") +
      geom_text(aes(y = 0.02, label = species), size = 4, color = "white", hjust = 0) +
      geom_text(aes(
        y = prediction_current + 0.05,
        label = paste0(round(prediction_current*100, 0), "%"),
        color = prediction_current),
        size = 4, hjust = 0) +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(species_prediction_df_small$species))) +
      expand_limits(y = c(0, 1.1)) +
      ggtitle("Most common species:") +
      theme_void() +
      theme(legend.position = "none")
  })

} # end server

shinyApp(ui, server)