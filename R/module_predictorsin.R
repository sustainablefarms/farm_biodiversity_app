# Module for all the land selections
predictors_UI <- function(id){
  ns <- NS(id)
  tagList(
    accordion(id = ns("acc"), 
      accordion_item("Select Location", id = ns("acc-loc"), selectlocationUI(ns("loc"))),
      accordion_item("Rainfall Since Last August", id = ns("acc-yfa"), selectYfAUI(ns("yfa"))),
      selectpatch_UI(ns("ptch"))
    ),
         if (isTRUE(getOption("shiny.testmode"))){
           downloadButton(ns("downloadcvals"), "Download Current Values", class = "download_badge")
         },
         if (isTRUE(getOption("shiny.testmode"))){
           actionButton(ns("viewcvals"), "View Current Values", class = "download_badge")
         }
  )
}

predictors_Server <- function(id, selected_region, newinattr){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      ## PATCH (and year)
      patchattr_tbl <- selectpatch_Server("ptch", selected_region, newinattr)
      frompatch  <- reactive({
        outinfo <- list()
        validate(need(patchattr_tbl(), "No attributes"))
        outinfo$patches <- nrow(patchattr_tbl())
        
        outinfo$woody500m = patchattr_tbl()$woody500m
        outinfo$woody3000m = patchattr_tbl()$woody3000m
        outinfo$noisy_miner = patchattr_tbl()$noisy_miner
        outinfo$IsRemnant = patchattr_tbl()$IsRemnant
    
        outinfo$year = 2018
        outinfo$allpatchcomplete = TRUE #obsolete
        outinfo
      }) 
    
      ## REGION
      fromlocation <- selectlocationServer("loc", selected_region)
      
      ## YfA
      fromyfa <- selectYfAServer("yfa", locationinfo = fromlocation)
      
      ## Combine!
      cval <- eventReactive({c(fromyfa(),
        frompatch())}, {
        out <- c(fromlocation(),
                 fromyfa(),
                 frompatch())
        out
      })
  
  ## Other!
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
          verbatimTextOutput(ns("cvals")),
          title = "Current Values for Prediction",
          size = "l",
          easyClose = TRUE,
        )
      )
    })
    output$cvals <- renderPrint({
      cval()
    })
  }
      
  setBookmarkExclude(c("viewcvals", "downloadcvals"))
    ## out!
      cval
    })
}

app_predictorsin <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  
  shinyApp(
    {bootstrapPage(
      includeCSS("./www/base.css"),
      predictors_UI("S1in"),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region <- reactiveVal("")
      newinattr <- reactiveVal(data.frame(pid = 1, 
                                          woody500m = 2.5,
                                          woody3000m = 3,
                                          noisy_miner = 1,
                                          IsRemnant = 1))
      refresh <- reactiveTimer(1000 * 10)
      observeEvent(refresh(),{
        attr <- newinattr()
        attr <- rbind(attr, attr[1, ])
        attr[1, "pid"] <- 3
        attr$woody500m <- 1.3 * attr$woody500m
        newinattr(attr)
      })
      predictors_Server("S1in", selected_region, newinattr)
      # observe(print(data.frame(reactiveValuesToList(cval1()))))
    })
}
