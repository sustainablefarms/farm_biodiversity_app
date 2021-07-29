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
  consstatus <<- load_constatus()
  appname <<- "Bird Checker: A Bird Occupancy Estimator"
  if (!isTRUE(getOption("shiny.testmode"))){
    appversion <<- as.character(packageVersion(packageName()))
  } else {appversion <<- "0.9"} #so that the same version number appears in all shinytest snapshot tests
  appurl <<- "https://sustfarm.shinyapps.io/bird_checker/"
  covarnicenames_tbl <<- read.csv("./data/nicecovarnames.csv", header = TRUE)
  apptempdir <<- tempdir()
  report_path <<- paste0(apptempdir, "/", "report.Rmd") #file location assumes host is a unix machine
  stopifnot(file.copy("data/report.Rmd", report_path, overwrite = TRUE)) 
  dir.create(paste0(apptempdir,"/www/"))
  stopifnot(file.copy("./www/Sustainable Farms logo RGB.png", paste0(apptempdir, "/www/"), overwrite = TRUE)) 
  stopifnot(file.copy(paste0("./www/", speciesinfo$imgfilename), paste0(apptempdir, "/www/"), overwrite = TRUE)) 
}

# UI
ui <- function(){
  out <- fluidPage(
    waiter::use_waiter(), 
    waiter::waiter_preloader(),
    includeCSS("./www/base.css"),
    # the following enables bootstrap 3's inbuilt tooltips
    tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
    ),
    shinyjs::useShinyjs(),
    HTML("<div class='header'>"),
    fluidRow(
      column(width = 2,
             tags$img(src = "Sustainable Farms logo RGB.png", alt = "logo", width = "100px")),    
    column(width = 10, offset = 0, 
      tags$span(class = 'main', appname),
      tags$span(style = "white-space:nowrap;", class = 'subtitle', "By Kassel Hingee & Martin Westgate.",
	       "Version", appversion),
      tags$span(class = 'subtitle', HTML("<br>Birds you can expect to see in spring in your farm's Box Gum Grassy Woodland remnants and plantings.")),
    actionButton2("intro", "Intro", class = "badge_tiny"),
    actionButton2("overallhelp", "More Help", class = "badge_tiny"),
           )),
    HTML("</div>"),
    column(width = 1,
	   tags$br(), tags$br(),
	   tags$br(), tags$br(),
      tags$div(tags$em("1. Set your region.")),
	   tags$br(), tags$br(),
      tags$div(tags$em("2. Set your woodland patches.")),
	   tags$br(), tags$br(),
      tags$div(tags$em("3. See your birds."))
	   ),
    column(width = 3,
      fluidRow(selectlocationUI("location")),
      # plotOutput("climate", height = "300px")
      fluidRow(selectYfAUI("yfa")),
      if (isTRUE(getOption("shiny.testmode"))){
        downloadButton("downloadcvals", "Download Current Values", class = "download_badge")
      },
      if (isTRUE(getOption("shiny.testmode"))){
        actionButton("viewcvals", "View Current Values", class = "download_badge")
      }
    ),
    column(width = 1),
    column(width = 6,
      fluidRow(
        selectpatchUI("patch")
      ),
      fluidRow(
        HTML("<div class='subheader'><h2>BIRD BIODIVERSITY</h2></div>"),
        tags$div(id = "predpanel", predictionsUI("pred", usedflt = NULL)) %>%
          shinyjs::hidden()
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
  fromyfa <- selectYfAServer("yfa", locationinfo = fromlocation)
  
  ## Combine!
  cval <- eventReactive({c(fromyfa(),
    reactiveValuesToList(frompatch))}, {
    out <- c(fromlocation(),
             fromyfa(),
           reactiveValuesToList(frompatch))
    out
  })
  if (isTRUE(getOption("shiny.testmode"))){
    observeEvent(cval(), {print("New cval() evaluation")
                          print(list2DF(cval()))})
    # cval(readRDS("./tests/testthat/current_values_1patch.rds"))
  }
  
  ## PREDICTIONS
  # reveal predictions panel
  observeEvent(cval(), {
    if (cval()$locationcomplete & cval()$allpatchcomplete){
      shinyjs::show("predpanel")
    } else {
      shinyjs::hide("predpanel")
    }
  })
  usedflt <- predictionsServer("pred", cval,
                    model_data,
                    report_path) 
  
  ## Help
  observeEvent(input$intro, {
    showModal(splashmodal())
    },
    ignoreNULL = FALSE)
  observeEvent(input$overallhelp, {
    showModal(moreinfomodal())
    },
    ignoreNULL = TRUE)
  if (isTRUE(getOption("shiny.testmode"))){
    output$downloadcvals <- downloadHandler(
      filename = "current_values.rds",
      content = function(file) {
        outdata <- cval()
        saveRDS(outdata, file)
      }
    )
    # modal more detail stuff
    observeEvent(input$viewcvals, {
      showModal(
        modalDialog(
          verbatimTextOutput("cvals"),
          title = "Current Values for Prediction",
          size = "l",
          easyClose = TRUE,
        )
      )
    })
    output$cvals <- renderPrint({
      cval()
    })
    
    #species images
    lapply(model_data$species, function(spec){
      output[[paste0("img_", gsub(" ", "-", spec))]] <- renderImage({
        list(src = speciesinfo[spec, "imgfilename"],
             alt = speciesinfo[spec, "imgfilename"],
             height = "200px")
      }, deleteFile = FALSE, quoted = FALSE)
    })
  }

} # end server


