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
  enableBookmarking(store = "url")
}

# UI
ui <- function(request){
  navbarsection <- navbarPage(title = "",
      tabPanel(title = "AccordTest",
               # includeHTML("./www/accordiontest.html")
               accordion(id = "taccordion", 
                 accordion_item("i1title", id = "i1", parentaccordionid = "taccordion", "This is the content")
               ),
               accordion(id = "t2accordion", 
                         accordion_item("2i1title", id = "2i1", parentaccordionid = "t2accordion", "This is the content2")
               )
      ),
      tabPanel(title = "Intro",
               startpage()
      ),
      tabPanel(title = "The Land 1",
               predictors_UI("S1in")
      ),
      tabPanel(title = "Estimates 1",
           predictionsUI("pred1")
      ),
      tabPanel(title = "The Land 2",
               predictors_UI("S2in")
      ),
      tabPanel(title = "Estimates 2",
           predictionsUI("pred2")
      ),
    collapsible = TRUE,
    footer = "Forward <-> back"
  )
  out <- bootstrapPage(
      waiter::use_waiter(), 
      waiter::waiter_preloader(),
      tags$head(includeHTML("./www/google-analytics.html")),
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
               linknewtab(href = "http://sustainablefarms.org.au/",
                          tags$img(src = "Sustainable Farms logo RGB.png", alt = "logo", width = "100px"))),    
        column(width = 10, offset = 0, 
               tags$span(class = 'main', appname),
               tags$span(style = "white-space:nowrap;", class = 'subtitle', "By Kassel Hingee & Martin Westgate.",
                         "Version", appversion),
               tags$span(class = 'subtitle', HTML("<br>Birds you can expect to see in spring in your farm's Box Gum Grassy Woodland remnants and plantings.")),
               actionButton2("intro", "Intro", class = "badge_tiny"),
               actionButton2("overallhelp", "More Help", class = "badge_tiny"),
        )),
      HTML("</div>"),
      navbarsection,
      title = appname,
      theme = bslib::bs_theme(version = 5, "lumen")
  )
}

# SERVER
server <- function(input, output, session) {

  # set up required data
  ## SF logo
  output$sflogo <- renderImage(
    list(src = "Sustainable Farms logo RGB.png",
         alt = "SF logo",
         height = "100px"),
       deleteFile = FALSE
  )
  
  ## Predictors Input
  cval1 <- predictors_Server("S1in")
  cval2 <- predictors_Server("S2in")
  
  if (isTRUE(getOption("shiny.testmode"))){
    observeEvent(cval1(), {print("New cval1() evaluation")
                          print(list2DF(cval1()))})
    observeEvent(cval2(), {print("New cval2() evaluation")
      print(list2DF(cval2()))})
    # cval1(readRDS("./tests/testthat/current_values_1patch.rds"))
  }
  
  ## Set up average situation -- could be performed offline
  modwmeanXocc <- msod::supplant_new_data(model_data, new_data_mean, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                                                  model_data$XoccProcess$scale,
                                                                                                  model_data$XoccColNames)})
  species_prob_mean <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwmeanXocc)
  species_prob_mean_r <- reactive({species_prob_mean})
  
  ## PREDICTIONS
  pred1arr <- predictionsServer("pred1", cval1,
                    species_prob_mean_r,
                    model_data,
                    report_path) 
  predictionsServer("pred2", cval2,
                    pred1arr,
                    model_data,
                    report_path) 
  
  ## Help
  observeEvent(input$overallhelp, {
    showModal(moreinfomodal())
    },
    ignoreNULL = TRUE)
  
  setBookmarkExclude(c("overallhelpfake",
                       "moredetailfake",
                       "overallhelp",
                       "waiter_shown",
                       "introfake",
                       "downloadreportfake",
                       "intro",
                       ".clientValue-default-plotlyCrosstalkOpts",
                       "plotly_click-region_map",
                       "plotly_relayout-region_map",
                       "plotly_afterplot-A",
                       "plotly_relayout-A",
                       "plotly_hover-A",
                       "plotly_afterplot-region_map",
                       "plotly_hover-region_map"))
  
  # Automatically bookmark every time an input changes
  # Update the query string
  onBookmarked(updateQueryString)

} # end server


