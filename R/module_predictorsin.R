# Module for all the land selections
predictors_UI <- function(id, isS2 = TRUE){
  ns <- NS(id)
  tagList(
    fluidRow(class = "justify-content-center",
             column(6, class = "text-center",
      if (isS2){
        tagList(
          tags$h1("Create a new scenario"),
          tags$h3("Step 3: Modify your farm to create a comparison"),
          tags$p("Use this step to increase woody vegetation cover,",
                  "add or remove woodland areas,",
                  "and add or remove Noisy Miners.",
                 "The information will become Scenario 2.")
        )
      } else {
        tagList(
          tags$h1("Your Farm"),
          tags$h3("Step 1: Tell us about your farm"),
          tags$p("Select your region,",
                   "then add the number of woodland areas found on your farm,",
                   "before defining the characteristics of each woodland area.",
                   "This information will become Scenario 1.")
        )
      }
      )
    ),
    accordion(id = ns("acc"), 
      selectlocationUI(ns("loc")),
      selectpatch_UI(ns("ptch")),
      opentype = "edit"
    ),
         if (isTRUE(getOption("shiny.testmode"))){
           downloadButton(ns("downloadcvals"), "Download Current Values", class = "download_badge")
         },
         if (isTRUE(getOption("shiny.testmode"))){
           actionButton(ns("viewcvals"), "View Current Values", class = "download_badge")
         }
  )
}

predictors_Server <- function(id, selected_region, newinattr, inAnnPrec.YfA){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      ## PATCH (and year)
      patchattr_tbl <- selectpatch_Server("ptch", selected_region, newinattr)
      frompatch  <- reactive({
        outinfo <- list()
        validate(need(patchattr_tbl(), "No attributes"))
        outinfo$patchattr_tbl = patchattr_tbl()
        outinfo
      }) 
      
      ## REGION
      fromlocation <- selectlocationServer("loc",
                                           selected_region,
                                           inAnnPrec.YfA)
      
      ## Combine!
      cval <- reactive({
        out <- c(fromlocation(),
                 list(patchattr_tbl = patchattr_tbl()))
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
      shinyjs::useShinyjs(),
      includeCSS("./www/base.css"),
      includeCSS("./www/accordion.css"),
      predictors_UI("S1in", isS2 = FALSE),
      theme = bslib::bs_theme(version = 5, "lumen"))
    },
    function(input, output, session){
      selected_region <- reactiveVal()
      newinattr <- reactiveVal(cbind(defaultpatchvalues, pid = 1))
      refresh1 <- reactiveTimer(1000 * 7)
      refresh2 <- reactiveTimer(1000 * 11)
      observeEvent(refresh1(),{newinattr(NULL); print("NULL inputs")})
      observeEvent(refresh2(),{newinattr(cbind(defaultpatchvalues, pid = 1)); print("1 patch in")})
      # refresh <- reactiveTimer(1000 * 10)
      # observeEvent(refresh(),{
      #   attr <- newinattr()
      #   if (!isTruthy(attr)){
      #     newinattr(data.frame(woody500m = 5, woody3000m = 5, noisy_miner = TRUE, IsRemnant = TRUE, pid = 1))
      #   } else {
      #     newattr <- attr[1, ]
      #     newattr$pid <- max(attr$pid) + 1
      #     attr <- rbind(attr, newattr)
      #     attr$woody500m <- 1.3 * attr$woody500m
      #     newinattr(attr)
      #   }         
      # print("new in attribute table:")
      # print(attr)
      # }, ignoreInit = TRUE)
      inAnnPrec.YfA <- reactiveVal()
      # observeEvent(refresh(), {
      #   inAnnPrec.YfA(inAnnPrec.YfA() + 50)
      #   selected_region("Euroa")
      # })
      predictors_Server("S1in", selected_region, newinattr, inAnnPrec.YfA)
      # observe(print(data.frame(reactiveValuesToList(cval1()))))
    })
}
