# Module for all the land selections
predictors_UI <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      tags$h1("Your Farm"),
      tags$h3("Step 1: Tell us about your farm"),
      tags$div("Select your region,",
                "then add the number of woodland areas found on your farm,",
                "before defining the characteristics of each woodland area.",
                "This information will become Scenario 1.",
               style = "text-align: center; max-width: 552px; margin: auto;")
    ),
    accordion(id = ns("acc"), 
      accordion_item("Your region", id = ns("acc-loc"), 
                     selectlocationUI(ns("loc")),
                     div(id = ns("yfa_wrap"), selectYfAUI(ns("yfa"))),
                     footer = tagList(
                       do.call(actionButton,
                               args = c(list(ns(paste0("cancel_region")), "Cancel", class = "btn-secondary"),
                                        toggle_attr(paste0(ns("acc-loc"), "_body"))
                               )),
                       do.call(actionButton,
                         args = c(list(ns(paste0("save_region")), "Save and Close", class = "btn-primary"),
                                  toggle_attr(paste0(ns("acc-loc"), "_body"))
                         ))
                     ),
                     footerdflt = "none"
                     ),
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
                                           inAnnPrec.YfA,
                                           reactive(input$save_region),
                                           reactive(input$cancel_region))
      
      
      ## Combine!
      cval <- eventReactive({c(fromyfa(),
                               patchattr_tbl())}, {
        out <- c(fromlocation(),
                 fromyfa(),
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
      includeCSS("./www/base.css"),
      includeCSS("./www/accordion.css"),
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
      refresh <- reactiveTimer(1000 * 30)
      # observeEvent(refresh(),{
      #   attr <- newinattr()
      #   attr <- rbind(attr, attr[1, ])
      #   attr[1, "pid"] <- 3
      #   attr$woody500m <- 1.3 * attr$woody500m
      #   newinattr(attr)
      # })
      inAnnPrec.YfA <- reactiveVal(400)
      observeEvent(refresh(), {
        inAnnPrec.YfA(inAnnPrec.YfA() + 50)
        selected_region("Euroa")
      })
      predictors_Server("S1in", selected_region, newinattr, inAnnPrec.YfA)
      # observe(print(data.frame(reactiveValuesToList(cval1()))))
    })
}
