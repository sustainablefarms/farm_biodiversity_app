# overarching UI and Server
myapp <- function(){
  main_app_prep() # loads things into global environment, prepares report file
  shinyApp(ui, server)
}

main_app_prep <- function(){  # loads things into global environment, prepares report file
  # Data Preparations
  model_data <<- load_model_data()
  new_data_mean <<- get_new_data_mean(model_data)
  
  # preptraits(model_data)
  loadtraits2global()
  load_birdinfotable()
  appname <<- "Draft Woodland Remnant Bird Biodiversity Estimator"
  covarnicenames_tbl <<- read.csv("./data/nicecovarnames.csv", header = TRUE)
  apptempdir <<- tempdir()
  report_path <<- paste0(apptempdir, "/", "report.Rmd") #file location assumes host is a unix machine
  stopifnot(file.copy("report.Rmd", report_path, overwrite = TRUE)) 
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
    HTML("<div class='header'>"),
    fluidRow(
      column(width = 2,
      imageOutput("sflogo", inline = TRUE)),    
    column(width = 10, offset = 0, HTML("
      <span class='main'>Woodland Remnant Bird Biodiversity Estimator</span>
      <span class='subtitle'>By Martin Westgate & Kassel Hingee. Version 0.2</span>
      <span><em><br>Set your region. Set your woodland patches. See your birds.</em></span>
    "),
    actionButton("overallhelp", "More Help", class = "download_badge"),
           )),
    HTML("</div>"),
    column(width = 1),
    column(width = 3,
      selectlocationUI("location"),
      # plotOutput("climate", height = "300px")
      selectYfAUI("yfa")
    ),
    column(width = 1),
    column(width = 6,
      fluidRow(
        selectpatchUI("patch")
      ),
      fluidRow(
        HTML("<div class='subheader'><h2>BIRD BIODIVERSITY</h2></div>"),
        uiOutput("pred")
      )
    ),
    title = appname,
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
  cval <- reactiveVal(
    value = NULL,
    label = "Current Predictor Values"
  )
  exportTestValues(selected_region = data$selected_region,
                   patches = current_values$patches) 
  ## SF logo
  output$sflogo <- renderImage(
    list(src = "Sustainable Farms logo RGB.png",
         alt = "SF logo",
         height = "100px"),
       deleteFile = FALSE
  )
  
  
  ## PATCH (and year)
  frompatch <- selectpatchServer("patch")

  ## REGION
  fromlocation <- selectlocationServer("location")
  
  ## YfA
  yfavals <- selectYfAServer("yfa")
  
  ## Combine!
  observe({
    cval(c(reactiveValuesToList(fromlocation), 
           reactiveValuesToList(yfavals),
           reactiveValuesToList(frompatch)))
  })
  
  ## PREDICTIONS
  output$pred <- renderUI({
    validate(need(cval()$locationcomplete & cval()$allpatchcomplete,
             ""))
    tagList(
      predictionsUI("pred"))
    })
  predictionsServer("pred", cval,
                    model_data, new_data_mean,
                    report_path)
  
  ## Help
  observeEvent(input$overallhelp, {
    showModal(modalDialog(
      "More detailed help. For queries email ...",
      modalButton("Close"),
      title = "More Help",
      easyClose = TRUE,
      fade = TRUE
      ))
    })

} # end server


