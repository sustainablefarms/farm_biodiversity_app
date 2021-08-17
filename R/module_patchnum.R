patchnum_UI <- function(id){
  ns <- NS(id)
  uiOutput(ns("patch_selector"))
}

patchnum_Server <- function(id, update, maxpatchnum){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      
  # number of patches
  output$patch_selector <- renderUI({
    tags$div(
      tags$script("$(function () {
          $('[data-toggle=tooltip]').tooltip()
        })"
      ),
    actionButton2(
      inputId = ns("choose_n_patches"),
      label = HTML(paste0("Number of<br>patches<br><h3>", update$numpatches_new, "</h3>")),
      class = "badge"
    ) 
    )
  })
  observeEvent(input$choose_n_patches, {
    showModal(
      modalDialog(
        radioButtons(ns("n_patches"),
                     label = "Number of woodland patches",
                     choiceNames = as.character(1:maxpatchnum),
                     choiceValues = 1:maxpatchnum,
                     inline = TRUE,
                     width = "100%",
                     selected = update$numpatches_existing),
        tags$br(),
	patchdefn,
	tags$br(),
        actionButton(ns("choose_n_patches_execute"), "Save"),
        modalButton("Cancel"),
        title = "Select number of patches",
        footer = NULL,
        easyClose = FALSE
      )
    )
  })

  # once number of patches is chosen, decide whether to add or subtract 'patch' buttons
  # to add dependence on restored
  choose_n_patches_execute_2 <- reactiveVal(0)
  observeEvent(input$choose_n_patches_execute, {
    choose_n_patches_execute_2(1 + choose_n_patches_execute_2())
  })
  observeEvent(choose_n_patches_execute_2(), {
    showNotification(paste("observeEvent:",
                     format(choose_n_patches_execute_2()),
                     input$n_patches))
    if (!is.null(input$n_patches) & (choose_n_patches_execute_2() > 0)){
    if (isTRUE(getOption("shiny.testmode"))){
      showNotification(input$n_patches, duration = 2)
    }
  
    update$numpatches_new <- as.integer(input$n_patches)
    if(update$numpatches_existing > update$numpatches_new){
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- TRUE
      update$remove_values <- seq_len(update$numpatches_existing)[-seq_len(update$numpatches_new)]
    }
    if(update$numpatches_existing == update$numpatches_new){
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- FALSE
      update$remove_values <- NULL
    }
    if(update$numpatches_existing < update$numpatches_new){
      update$add_logical <- TRUE
      update$add_values <- seq_len(update$numpatches_new)[-seq_len(update$numpatches_existing)]
      update$remove_logical <- FALSE
      update$remove_values <- NULL
    }}
    removeModal()
  }, ignoreInit = TRUE)
  
  
  # Bookmarking code
  setBookmarkExclude(c("choose_n_patches",
                       "n_patches",
                       "choose_n_patches_execute"))
  
  # Save extra values in state$values when we bookmark
  onBookmark(function(state) {
    # state$values$update <- reactiveValuesToList(update)
    state$values$numpatches_new <- update$numpatches_new
  })
  
  # Read values from state$values when we restore. Do it after app loading
  onRestored(function(state) {
    updateRadioButtons(session, inputId = "n_patches", selected = state$values$numpatches_new)
    choose_n_patches_execute_2(1 + choose_n_patches_execute_2())
    showNotification(paste("onRestored:",
                           format(state$values$numpatches_new),
                           format(input$n_patches)))
  })

  
  update
    }
  )}