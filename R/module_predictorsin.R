# Module for all the land selections
predictors_UI <- function(id){
  ns <- NS(id)
  tagList(
         fluidRow(selectlocationUI(ns("loc"))),
         fluidRow(selectYfAUI(ns("yfa"))),
         fluidRow(
           selectpatchUI(ns("ptch"))
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
      frompatch <- selectpatchServer("ptch", selected_region)
    
      ## REGION
      fromlocation <- selectlocationServer("loc")
      selected_region <- reactive({fromlocation()$selected_region})
      
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
  }
      
  setBookmarkExclude(c("viewcvals", "downloadcvals"))
    ## out!
      cval
    })
  }