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

predictors_Server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      ## PATCH (and year)
      frompatch <- selectpatch_Server("ptch", selected_region)
    
      ## REGION
      fromlocation <- selectlocationServer("loc")
      selected_region <- reactive({fromlocation()$selected_region})
      
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
      predictors_Server("S1in")
      # observe(print(data.frame(reactiveValuesToList(cval1()))))
    })
}
