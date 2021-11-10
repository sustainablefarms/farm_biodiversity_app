# overarching UI and Server
myapp <- function(){
  main_app_prep() # loads things into global environment, prepares report file
  shinyApp(ui, server)
}

main_app_prep <- function(){  # loads things into global environment, prepares report file
  # Data Preparations
  model_data <<- load_model_data()
  new_data_mean <<- get_new_data_mean(model_data)  
  ## Set up average situation -- could be performed offline
  modwmeanXocc <- msod::supplant_new_data(model_data, new_data_mean, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                                                  model_data$XoccProcess$scale,
                                                                                                  model_data$XoccColNames)})
  species_prob_mean <<- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwmeanXocc)
  
  # preptraits(model_data)
  loadtraits2global()
  load_birdinfotable()
  bbox_allregions <<- readRDS("./data/sa2_polygons_bboxtotal.rds")
  bbox_regions <<- readRDS("./data/sa2_polygons_bbox.rds")
  defaultpatchvalues <<- data.frame(woody500m = round(new_data_mean$WCF_500/0.5) * 0.5,
                              woody3000m = round(new_data_mean$WCF_3000/0.5) * 0.5)
  
  consstatus <<- load_constatus()
  appname <<- "BirdCast"
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
  appcolors <<- c("Dark Green" = "#026666",
              "Dark Gray" = "#3B4042",
              "Bright Blue" = "#168BCB")
  enableBookmarking(store = "disable")
}

# UI
ui <- function(request){
  navbarsection <- tabsetPanel(
      tabPanelBody(value = "in1",
               predictors_UI("S1in", isS2 = FALSE),
               fluidRow(column(6, actionButton("in1_back", "Back", class = "btn-secondary", width = "100%")),
                        column(6, actionButton("in1_next", "Next", class = "btn-primary", width = "100%")))
      ),
      tabPanelBody(value = "out1",
           predictionsUI("pred1", refisaverage = TRUE),
           fluidRow(class = "align-items-end",
            column(4, class = "text-center",
             tags$h2("Edit your farm data"),
             tags$div(class = "bodysmall", "Go back to edit Scenario 1"),
             actionButton("out1_back", "Back", class = "btn-secondary", width = "100%")
            ),
            column(4, class = "text-center",
             tags$div(style = paste0("background-color: ", appcolors[["Bright Blue"]], ";"),
                      style = "height: 100%",
             tags$h2("Download a report"),
             tags$div(class = "bodysmall", 
                      "Download a full report on the birds that are likely to live in your farm's woodland.",
                      "This report will include comparison between your farm and bird occupancy in an average woodland area.")
             ),
             actionButton("out1_product", "Download Report", width = "100%", style = paste0("background-color: ", appcolors[["Bright Blue"]], ";") )
            ),
            column(4, class = "text-center",
             tags$h2("Create a comparison"),
             tags$div(class = "bodysmall",
                      "Go to the next step to create a second comparison scenario for your farm.",
                      "For example, what birds might live on your farm if you increase woody vegetation cover?"),
             actionButton("out1_next", "Next", class = "btn-primary", width = "100%")
            )
          )
      ),
      tabPanelBody(value = "in2",
               predictors_UI("S2in", isS2 = TRUE),
               fluidRow(column(6, actionButton("in2_back", "Back", class = "btn-secondary", width = "100%")),
                        column(6, actionButton("in2_next", "Next", class = "btn-primary", width = "100%")))
      ),
      tabPanelBody(value = "out2",
           predictionsUI("pred2", refisaverage = FALSE),
           fluidRow(class = "align-items-end",
            column(4, class = "text-center",
             tags$h2("Edit your farm data"),
             tags$div(class = "bodysmall", "Go back to edit Scenario 2"),
             actionButton("out2_back", "Back", class = "btn-secondary", width = "100%")
            ),
            column(4, class = "text-center",
             tags$div(style = paste0("background-color: ", appcolors[["Bright Blue"]], ";"),
                      style = "height: 100%",
             tags$h2("Download a report"),
             tags$div(class = "bodysmall", 
                      "Download a full report on the birds that are likely to live in your farm's woodland.",
                      "This report will include comparison between Scenario 1 and Scenario 2.")
             ),
             actionButton("out2_product", "Download Report", width = "100%", style = paste0("background-color: ", appcolors[["Bright Blue"]], ";") )
            ),
            column(4, class = "text-center",
             tags$h2("Estimation Complete"),
             tags$div(class = "bodysmall",
                      "Congratulations! You have completed all the steps in the app.",
                      "Visit the Sustainable Farms website for more guidance on supporting sustainable and profitable agriculture."),
             actionButton("out2_next", "Go to Sustainable Farms", class = "btn-primary", width = "100%")
            )
          )
      ),
    id = "maintabs",
    type = "hidden"
  )
  out <- bootstrapPage(
      includeCSS("./www/base.css"),
      includeCSS("./www/accordion.css"),
      waiter::use_waiter(), 
      tags$head(includeHTML("./www/google-analytics.html")),
      tags$script("$(function () {
        $('[data-toggle=tooltip]').tooltip()
      })"
      ),
      shinyjs::useShinyjs(),
      leaflet::leafletOutput("loadleaflet", height = "0px", width = "0px"), #so leaflet scripts are loaded
      plotly::plotlyOutput("loadplotly", height = "0px", width = "0px"), #so plotly is loaded
      headercontent(),
      tags$div(id = "lp", landingpage()),
      tags$div(id = "tw", class = "visually-hidden", navbarsection),
      footercontent(),
      title = appname,
      theme = bslib::bs_theme(version = 5, "lumen",
                              "primary" = appcolors[["Dark Green"]],
                              "dark" = appcolors[["Dark Gray"]])
  )
}

# SERVER
server <- function(input, output, session) {
  ns <- session$ns

  
  ## Stuff to do with the opening page of the app
  removeUI(selector = paste0("#", ns("startspinner")))
  insertUI(selector = paste0("#", ns("startbuttonlocation")),
           where = "afterBegin",
           ui = actionButton("hidestartpage", "Start", class = "position-absolute btn-primary translate-middle"))
  observeEvent(input$hidestartpage, {
    shinyjs::addClass(class = "visually-hidden", selector = "#lp")
    shinyjs::removeClass(class = "visually-hidden", selector = "#tw")
  })
  
  # set up required data
  startregion <- reactiveVal("") #so region select box starts at ""
  startattr <- reactiveVal(cbind(defaultpatchvalues, pid = 1)) #this is duplicated in restart
  startAnnPrec.YfA <- reactiveVal(NULL)
  inregion <- reactiveVal()
  inattr <- reactiveVal()
  inAnnPrec.YfA <- reactiveVal() 
  
  ## SF logo
  output$sflogo <- renderImage(
    list(src = "Sustainable Farms logo RGB.png",
         alt = "SF logo",
         height = "100px"),
       deleteFile = FALSE
  )
  
  ## Predictors Input
  cval1 <- predictors_Server("S1in", startregion,  startattr,  startAnnPrec.YfA)
  cval2 <- predictors_Server("S2in",  inregion,  inattr,  inAnnPrec.YfA)
  
  # if (isTRUE(getOption("shiny.testmode"))){
  #   observeEvent(cval1(), {print("New cval1() evaluation")
  #                         print(cval1())})
  #   observeEvent(cval2(), {print("New cval2() evaluation")
  #     print(cval2())})
  #   # cval1(readRDS("./tests/testthat/current_values_1patch.rds"))
  # }
  
  # populating Scenario 2
  observeEvent(input$out1_next, {
    inregion(cval1()$selected_region)
    inAnnPrec.YfA(cval1()$AnnPrec.YfA)
    inattr(cval1()$patchattr_tbl)
  })
  

  
  ## PREDICTIONS
  pred1arr <- predictionsServer("pred1", cval1,
                    reactiveVal(species_prob_mean),
                    model_data,
                    report_path,
                    refisaverage = TRUE) 
  predictionsServer("pred2", cval2,
                    pred1arr,
                    model_data,
                    report_path,
                    refisaverage = FALSE) 
  
  ## Help
  observeEvent(input$overallhelp, {
    showModal(moreinfomodal())
    },
    ignoreNULL = TRUE)
  
  # restart modal
  observeEvent(input$restartmodal, {
    showModal(
      restartmodaldialog()
    )
  })
  
  # restart, set default starting too
  observeEvent(input$restart, {# need to flip them to something briefly observers notice a change
    shinyjs::addClass(class = "visually-hidden", selector = "#tw")
    shinyjs::removeClass(class = "visually-hidden", selector = "#lp")
    startregion(NULL)
    startregion("")
    startattr(0)
    startattr(cbind(defaultpatchvalues, pid = 1)) #this is duplicated in initiation of values
    startAnnPrec.YfA("")
    startAnnPrec.YfA(new_data_mean$AnnPrec.YfA)
  }, ignoreInit = TRUE, ignoreNULL = TRUE) #ignore init and null here so that I have a chane of making bookmarking work
  
  ## tab navigation
  observeEvent(input$in1_next, {updateTabsetPanel(session, inputId = "maintabs", "out1")}, ignoreInit = TRUE)
  
  observeEvent(input$out1_back, {updateTabsetPanel(session, inputId = "maintabs", "in1")}, ignoreInit = TRUE)
  observeEvent(input$out1_next, {updateTabsetPanel(session, inputId = "maintabs", "in2")}, ignoreInit = TRUE)
  
  observeEvent(input$in2_back, {updateTabsetPanel(session, inputId = "maintabs", "out1")}, ignoreInit = TRUE)
  observeEvent(input$in2_next, {updateTabsetPanel(session, inputId = "maintabs", "out2")}, ignoreInit = TRUE)
  
  observeEvent(input$out2_back, {updateTabsetPanel(session, inputId = "maintabs", "in2")}, ignoreInit = TRUE)
  
  
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


