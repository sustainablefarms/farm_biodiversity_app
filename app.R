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
    # plotOutput("climate", height = "300px")
  ),
  column(width = 1),
  column(width = 6,
    fluidRow(
      selectpatchUI("patch")
    ),
    fluidRow(
      predictionsUI("pred")
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
    selected_region = NULL,
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

  ## PATCH (and year)
  frompatch <- selectpatchServer("patch")
  observe({
    current_values$patches <- frompatch$patches
    current_values$woody_veg <- frompatch$woody_veg
    current_values$midstorey <- frompatch$midstorey
    current_values$noisy_miner <- frompatch$noisy_miner
    current_values$year <- frompatch$year
  })

  ## REGION
  outOfModule <- selectlocationServer("location")
  observe({
    data$selected_region <- outOfModule$selected_region
    current_values$AnnPrec <- outOfModule$AnnPrec
    current_values$MaxTWarmMonth                   <- outOfModule$MaxTWarmMonth
    current_values$MinTColdMonth                   <- outOfModule$MinTColdMonth
    current_values$PrecSeasonality                   <- outOfModule$PrecSeasonality
    current_values$latitude  <- outOfModule$latitude
  })

  ## PREDICTIONS
  predictionsServer("pred", data, current_values,
                    model_data, new_data_mean)

} # end server

shinyApp(ui, server)
