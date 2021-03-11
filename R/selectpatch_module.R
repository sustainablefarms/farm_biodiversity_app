selectpatchUI <- function(id){
  ns <- NS(id)
  tagList(
      HTML("<div class='subheader'><h2>FARM</h2></div>"),
      verbatimTextOutput("text"),
      column(width = 3,
        uiOutput(ns("patch_selector")),
        uiOutput(ns("year_selector"))
      ),
      column(width = 1),
      column(width = 8,
        actionButton2(
          inputId = ns("patch_number_1"),
          label = "Patch #1",
          class = "patch_badge"),
        tippy::tippy_this(ns("patch_number_1"), 
                  paste('<div style="text-align: left">A patch is a region of woodland vegetation that is',
                  '<br>(1) 1ha - 10ha (TBC) in area, ',
                  '<br>(2) has similar vegetation structure throughout, and',
                  '<br>(3) is approximately 50m from other woody vegetation.',
                  '<br><br> Currently the patch must be remnant box gum grassy woodlands.</div>'),
                  arrow = TRUE,
                  interactive = TRUE,
                  allowHTML = "true"),
                  # arrow = "roundArrow"),
        div(id = "placeholder")
      ))
}

selectpatchServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      # set up reactive values
      data <- reactiveValues(
        species_predictions = NULL)
      current_values <- reactiveValues(
        patches = 1,
        woody_veg = NA,
        midstorey = NA,
        noisy_miner = NA,
        year = 2018,
        AnnPrec = NULL,
        MaxTWarmMonth = NULL,
        MinTColdMonth = NULL,
        PrecSeasonality = NULL,
        latitude = NULL,
        AnnPrec = NULL,
        AnnTempRange = NULL,
        PrecSeasonality = NULL,
        PrecWarmQ = NULL)
      previous_values <- reactiveValues(
        patches = 1,
        patch_buttons = c(0),
        selected_patch = NULL)
      update <- reactiveValues(
        add_logical = FALSE,
        add_values = NULL,
        remove_logical = FALSE,
        remove_values = NULL)
      click_values <- reactiveValues(
        patches = NULL)
      
  ## FARM

  # number of patches
  output$patch_selector <- renderUI({
    shinyBS::tipify(actionButton2(
      inputId = ns("choose_n_patches"),
      label = HTML(paste0("Number of<br>patches<br><h3>", current_values$patches, "</h3>")),
      class = "badge"
    ), 
    paste('<div style="text-align: left">A patch is a region of woodland vegetation that <br>(1) is 1ha - 10ha <em>(TBC)</em> in area, <br>(2) has similar vegetation structure throughout, and <br>(3) is approximately 50m from other woody vegetation.',
    '<br><br> Currently the patch must be remnant box gum grassy woodlands.</div>'),
    options = list(html = TRUE)
    )
  })
  observeEvent(input$choose_n_patches, {
    showModal(
      modalDialog(
        sliderInput(
          inputId = ns("n_patches"),
          label = "Number of vegetation patches",
          min = 1, max = 6, step = 1, value = current_values$patches),
        actionButton(ns("choose_n_patches_execute"), "Save"),
        modalButton("Cancel"),
        title = "Select number of patches",
        footer = NULL,
        easyClose = FALSE
      )
    )
  })

  # once number of patches is chosen, decide whether to add or subtract 'patch' buttons
  observeEvent(input$choose_n_patches_execute, {
    current_values$patches <- input$n_patches
    if(previous_values$patches > current_values$patches){
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- TRUE
      update$remove_values <- seq_len(previous_values$patches)[-seq_len(current_values$patches)]
    }
    if(previous_values$patches == current_values$patches){
      update$add_logical <- FALSE
      update$add_values <- NULL
      update$remove_logical <- FALSE
      update$remove_values <- NULL
    }
    if(previous_values$patches < current_values$patches){
      update$add_logical <- TRUE
      update$add_values <- seq_len(current_values$patches)[-seq_len(previous_values$patches)]
      update$remove_logical <- FALSE
      update$remove_values <- NULL
    }
    removeModal()
  })

  # add 'patch' buttons
  observeEvent(update$add_values, {
    if(!is.null(input$n_patches) & update$add_logical){
      lapply(update$add_values, function(a){
        add_reference_ui(
          entry_number = a,
          ui_selector = "placeholder",
          ns
        )
      })
      update$add_logical <- FALSE
      previous_values$patches <- current_values$patches
      current_values$woody_veg[update$add_values] <- NA
      current_values$midstorey[update$add_values] <- NA
      current_values$noisy_miner[update$add_values] <- NA
    }
  })

  # substract 'patch' buttons
  observeEvent(update$remove_values, {
    if(!is.null(input$n_patches) & update$remove_logical){
      lapply(update$remove_values, function(a){
        removeUI(
          selector = paste0("#patch_number_", a)  #the '#' here tells jQuery to find the UI element based on element id.
        )
      })
      update$remove_logical <- FALSE
      previous_values$patches <- current_values$patches
      current_values$woody_veg <- current_values$woody_veg[-update$remove_values]
      current_values$midstorey <- current_values$midstorey[-update$remove_values]
      current_values$noisy_miner <- current_values$noisy_miner[-update$remove_values]
    }
  })

  # prediction year
  output$year_selector <- renderUI({
    actionButton2(
      inputId = ns("choose_prediction_year"),
      label = current_values$year, #HTML(paste0("<h3>", current_values$year, "</h3>")),
      class = "badge_small"
    )
  })
  observeEvent(input$choose_prediction_year, {
    showModal(
      modalDialog(
        selectInput(
          inputId = ns("choose_year"),
          label = "Prediction Year",
          choices = seq(2001, 2018, 1),
          selected = current_values$year,
          width = "100%"
        ),
        actionButton(ns("choose_prediction_year_execute"), "Save"),
        modalButton("Cancel"),
        title = "Select prediction year",
        footer = NULL,
        easyClose = FALSE
      )
    )
  })
  observeEvent(input$choose_prediction_year_execute, {
    current_values$year <- as.numeric(input$choose_year)
    removeModal()
  })

  # output$text <- renderPrint({
  #   # paste(
  #   #   paste(current_values$mistorey, collapse = "; "),
  #      paste(current_values$woody_veg, collapse = "; ")
  #   #   paste(current_values$noisy_miner, collapse = "; "),
  #   # collapse = "  |   ")
  # })
  # output$text <- renderPrint({previous_values$selected_patch})
  # output$text <- renderPrint({paste(current_values$woody_veg, collapse = "; ")})
  # output$text <- renderPrint({str(click_values$patches)})

  # for each patch, launch a modal to set new values
  observe({
    click_values$patches <- input_tracker(
      input = input,
      string = "patch_number_[[:digit:]]+"
    )
    if(nrow(click_values$patches) == length(previous_values$patch_buttons)){
      update_check <- (click_values$patches$value > previous_values$patch_buttons)
      if(any(update_check)){
        previous_values$selected_patch <- click_values$patches$id[which(update_check)]
        patch_modal(
          value = previous_values$selected_patch,
          woody_veg = current_values$woody_veg[previous_values$selected_patch],
          midstorey = current_values$midstorey[previous_values$selected_patch],
          noisy_miner = current_values$noisy_miner[previous_values$selected_patch],
          ns
        )
      }
    }
    previous_values$patch_buttons <- click_values$patches$value
  })

  # collect input values from modal
  observeEvent(input$choose_patch_attributes_execute, {
    current_values$woody_veg[previous_values$selected_patch] <- input[[
      paste0("pc_woody_veg_", previous_values$selected_patch)]]
    current_values$midstorey[previous_values$selected_patch] <- input[[
      paste0("pc_midstorey_", previous_values$selected_patch)]]
    current_values$noisy_miner[previous_values$selected_patch] <- input[[
      paste0("noisy_miner_", previous_values$selected_patch)]]
    removeModal()
  })
  
  current_values

  }
)
}
