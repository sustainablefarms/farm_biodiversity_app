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
  appcss <<- compilecss()
  enableBookmarking(store = "url")
}

# UI
tabwrapper <- function(){tabsetPanel(
    tabPanelBody(value = "in1",
             predictors_UI("S1in", isS2 = FALSE),
             fluidRow(style="margin-top: 2rem; margin-bottom: 2rem;",
		      column(6, actionButton_notdfl("in1_back", "Back", class = "btn-outline-primary py-3", width = "100%")),
                      column(6, actionButton_notdfl("in1_next", "Next", class = "btn-primary py-3", width = "100%")))
    ),
    tabPanelBody(value = "out1",
         predictionsUI("pred1", refisaverage = TRUE),
         out1_foot()
    ),
    tabPanelBody(value = "in2",
             predictors_UI("S2in", isS2 = TRUE),
             fluidRow(style="margin-top: 2rem; margin-bottom: 2rem;",
		      column(6, actionButton_notdfl("in2_back", "Back", class = "btn-outline-primary py-3", width = "100%")),
                      column(6, actionButton_notdfl("in2_next", "Next", class = "btn-primary py-3", width = "100%")))
    ),
    tabPanelBody(value = "out2",
         predictionsUI("pred2", refisaverage = FALSE),
         out2_foot()
    ),
  id = "maintabs",
  type = "hidden"
)
}

outerpage <- function(){bootstrapPage(
    tags$head(tags$style(appcss)),
    # includeCSS("./www/base.css"),
    # includeCSS("./www/accordion.css"),
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
    tags$div(id = "tw", class = "visually-hidden mx-2", tabwrapper()),
    footercontent(),
    title = appname,
    theme = apptheme()
)}

ui <- function(request){
  outerpage()
}

# SERVER
server <- function(input, output, session) {
  ns <- session$ns
  landingpagestatus <- reactiveVal(TRUE) #indicates whether page is open (TRUE) or closed (FALSE)

  
  ## Stuff to do with the opening page of the app
  removeUI(selector = paste0("#", ns("startspinner")))
  insertUI(selector = paste0("#", ns("startbuttonlocation")),
           where = "afterBegin",
           ui = actionButton_notdfl("hidestartpage", "Start", class = "position-absolute btn-primary translate-middle"))
  observeEvent(input$hidestartpage, {
    landingpagestatus(closelandingpage())
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
    landingpagestatus(openlandingpage())
    updateTabsetPanel(session, inputId = "maintabs", "in1")
    startregion(NULL)
    startregion("")
    startattr(0)
    startattr(cbind(defaultpatchvalues, pid = 1)) #this is duplicated in initiation of values
    startAnnPrec.YfA("")
    startAnnPrec.YfA(new_data_mean$AnnPrec.YfA)
    updateQueryString("?_inputs_")
  }, ignoreInit = TRUE, ignoreNULL = TRUE) #ignore init and null here so that I have a chane of making bookmarking work
  
  ## tab navigation
  observeEvent(input$in1_back, {
    landingpagestatus(openlandingpage())
  }, ignoreInit = TRUE)
  observeEvent(input$in1_next, {
    assessments <- checkcvals(cval1())
    if (length(assessments) > 0){
      lapply(assessments, function(str) showNotification(str, type = "error"))
    } else {
      updateTabsetPanel(session, inputId = "maintabs", "out1")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$out1_back, {updateTabsetPanel(session, inputId = "maintabs", "in1")}, ignoreInit = TRUE)
  observeEvent(input$out1_next, {updateTabsetPanel(session, inputId = "maintabs", "in2")}, ignoreInit = TRUE)
  
  observeEvent(input$in2_back, {updateTabsetPanel(session, inputId = "maintabs", "out1")}, ignoreInit = TRUE)
  observeEvent(input$in2_next, {
    assessments <- checkcvals(cval2())
    if (length(assessments) > 0){
      lapply(assessments, function(str) showNotification(str, type = "error"))
    } else {
      updateTabsetPanel(session, inputId = "maintabs", "out2")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$out2_back, {updateTabsetPanel(session, inputId = "maintabs", "in2")}, ignoreInit = TRUE)
 

  # bookmarking 
  # appendinputids() #for recording input ids - an observer
  observeEvent({
    c(input$hidestartpage,
      input$maintabs,
      input$out1_next)
    }, {
    showNotification("Bookmarking from main.R")
    session$doBookmark()
  }, ignoreInit = TRUE, priority = -100)
  
  # Update the query string - works for whole app I think
  onBookmarked(function(querystring){
    newstring <- minimisequerystring(querystring)
    updateQueryString(newstring)
    })
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    state$values$lp <- landingpagestatus()
    state$values$sr <- startregion()
    state$values$ir <- inregion()
    state$values$sp <- startAnnPrec.YfA()
    state$values$ip <- inAnnPrec.YfA()
    state$values$s1at <- compactattrtable(cval1()$patchattr_tbl)
    state$values$s2at <- compactattrtable(cval2()$patchattr_tbl)
  })
  
  # Read values from state$values when we restore
  onRestore(function(state) {
    if (length(state$values) > 0){
      # url converts "" values to list() values so below needed to fix it
      sr <- state$values$sr
      ir <- state$values$ir
      if (length(sr) == 0){ startregion("") } else { startregion(sr) }
      if (length(ir) == 0){ inregion("") } else { inregion(ir) }
      startAnnPrec.YfA(state$values$sp)
      inAnnPrec.YfA(state$values$ip)
      startattr(urltable2attrtbl(state$values$s1at))
      inattr(urltable2attrtbl(state$values$s2at))
      landingpagestatus(closelandingpage())
    }
  })

} # end server


