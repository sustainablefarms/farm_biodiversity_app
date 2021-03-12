# overarching UI and Server
myapp <- function(){
  # Data Preparations
  model_data <<- load_model_data()
  new_data_mean <<- get_new_data_mean(model_data)
  
  # preptraits(model_data)
  loadtraits2global()
  tempdir <- tempdir()
  report_path <<- paste0(tempdir, "/", "report.Rmd") #file location assumes host is a unix machine
  stopifnot(file.copy("report.Rmd", report_path, overwrite = TRUE)) 
  
  shinyApp(ui, server)
}
  
# UI
ui <- function(){
  out <- fluidPage(
    includeCSS("./www/base.css"),
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    # the following enables bootstrap 3's inbuilt popovers
    tags$script("$(function () {
      $('[data-toggle=popover]').popover()
    })"),
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
    column(width = 10, offset = 0, 
    HTML("<span class='main'>Woodland Remnant Bird Biodiversity Estimator</span>",
      '<p>This <a href="#" role="button" title="" data-toggle="popover" data-content="And heres some amazing content. Its very engaging. right?" data-original-title="A Title">button</a> should trigger a popover on click.</p>',
      "<span class='subtitle' data-toggle='tooltip' title='Some tooltip text!'><br>Version 0.2 (DRAFT). By Martin Westgate & Kassel Hingee</span>",
      '<button type="button" class="btn btn-lg btn-danger" data-toggle="popover" title="Popover title" data-content="And heres some amazing content. Its very engaging. Right?">Click to toggle popover</button>'
    ))),
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
    theme = bslib::bs_theme(version = 3, "lumen")
    # theme = shinytheme("lumen")
  )
}

# SERVER
server <- function(input, output, session) {

  # set up required data
  # 1. from model
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
  exportTestValues(selected_region = data$selected_region,
                   patches = current_values$patches) 

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
  predictionsServer("pred", current_values,
                    model_data, new_data_mean,
                    report_path)

} # end server


