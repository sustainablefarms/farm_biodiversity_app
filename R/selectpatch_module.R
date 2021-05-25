selectpatchUI <- function(id){
  ns <- NS(id)
  tagList(
      HTML("<div class='subheader'><h2>FARM</h2></div>"),
      verbatimTextOutput("text"),
      column(width = 3,
        uiOutput(ns("patch_selector")),
      ),
      column(width = 1),
      column(width = 8,
       div(
        actionButton2(
          inputId = ns("patch_number_1"),
          label = tags$span("Patch #1",
	     htmlOutput(outputId = ns("patch_num_complete_1"), inline = TRUE)),
          class = "patch_badge"),
        `data-toggle`="tooltip",
        `data-html` = TRUE,
        `data-placement` = "bottom",
        title = patchdefn
        ),
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
        allpatchcomplete = FALSE,
        patches = 1,
        woody500m = NA,
        woody3000m = NA,
        noisy_miner = NA,
        IsRemnant = NA,
        year = 2018)
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

  # orange tick for patch number 1
  output[["patch_num_complete_1"]] <- renderUI({
	tags$span(class="glyphicon glyphicon-warning-sign", 
		  style = "color: #CC9900;",
		  tabindex = "0",
		  `data-toggle` = "tooltip",
		  title = "Please set the attributes of this patch.",
		  `data-trigger` = "focus hover",
		  `data-placement` = "auto"
		  )})

  # number of patches
  output$patch_selector <- renderUI({
    tags$div(
      tags$script("$(function () {
          $('[data-toggle=tooltip]').tooltip()
        })"
      ),
    tags$div(actionButton2(
      inputId = ns("choose_n_patches"),
      label = HTML(paste0("Number of<br>patches<br><h3>", current_values$patches, "</h3>")),
      class = "badge"
    )), 
    `data-toggle`="tooltip",
    `data-html` = TRUE,
    `data-placement` = "auto bottom",
    `data-container` = "body", #this made the width of the tooltip better
    `data-vieport` = '#viewport',
    title = patchdefn,
    )
  })
  observeEvent(input$choose_n_patches, {
    showModal(
      modalDialog(
        radioButtons(ns("n_patches"),
                     label = "Number of woodland patches",
                     choiceNames = as.character(1:6),
                     choiceValues = 1:6,
                     inline = TRUE,
                     width = "100%",
                     selected = current_values$patches),
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
  observeEvent(input$choose_n_patches_execute, {
    current_values$patches <- as.integer(input$n_patches)
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
      # add orange tick
      lapply(update$add_values, function(a){
	output[[paste0("patch_num_complete_", a)]] <- renderUI({
			tags$span(class="glyphicon glyphicon-warning-sign", 
				  style = "color: #CC9900;"
				  )})
      })
      update$add_logical <- FALSE
      previous_values$patches <- current_values$patches
      current_values$woody500m[update$add_values] <- NA
      current_values$woody3000m[update$add_values] <- NA
      current_values$noisy_miner[update$add_values] <- NA
      current_values$IsRemnant[update$add_values] <- NA
      current_values$allpatchcomplete <- FALSE
    }
  })

  # substract 'patch' buttons
  observeEvent(update$remove_values, {
    if(!is.null(input$n_patches) & update$remove_logical){
      lapply(update$remove_values, function(a){
        removeUI(
          selector = paste0("#patch_number_", a)  #the '#' here tells jQuery to find the UI element based on element id.
	  )
	removeUI(
          selector = paste0("#patch_num_complete_", a)  #the '#' here tells jQuery to find the UI element based on element id.
        )
      })
      update$remove_logical <- FALSE
      previous_values$patches <- current_values$patches
      current_values$woody500m <- current_values$woody500m[-update$remove_values]
      current_values$woody3000m <- current_values$woody3000m[-update$remove_values]
      current_values$noisy_miner <- current_values$noisy_miner[-update$remove_values]
      current_values$IsRemnant <- current_values$IsRemnant[-update$remove_values]
    }
  })

  # output$text <- renderPrint({
  #   # paste(
  #   #   paste(current_values$mistorey, collapse = "; "),
  #      paste(current_values$woody500m, collapse = "; ")
  #   #   paste(current_values$noisy_miner, collapse = "; "),
  #   # collapse = "  |   ")
  # })
  # output$text <- renderPrint({previous_values$selected_patch})
  # output$text <- renderPrint({paste(current_values$woody500m, collapse = "; ")})
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
          woody500m = current_values$woody500m[previous_values$selected_patch],
          woody3000m = current_values$woody3000m[previous_values$selected_patch],
          noisy_miner = current_values$noisy_miner[previous_values$selected_patch],
          IsRemnant = current_values$IsRemnant[previous_values$selected_patch],
          ns
        )
      }
    }
    previous_values$patch_buttons <- click_values$patches$value
  })

  # collect input values from modal
  observeEvent(input$choose_patch_attributes_execute, {
    current_values$woody500m[previous_values$selected_patch] <- input[[
      paste0("pc_woody500m_", previous_values$selected_patch)]]
    current_values$woody3000m[previous_values$selected_patch] <- input[[
      paste0("pc_woody3000m_", previous_values$selected_patch)]]
    current_values$noisy_miner[previous_values$selected_patch] <- input[[
      paste0("noisy_miner_", previous_values$selected_patch)]]
    current_values$IsRemnant[previous_values$selected_patch] <- input[[
      paste0("IsRemnant_", previous_values$selected_patch)]]

    # add a green tick
    output[[paste0("patch_num_complete_", previous_values$selected_patch)]] <- renderUI({
			tags$span(class="glyphicon glyphicon-ok", 
				  style = "color: white;"
				  )})

    removeModal()

    
    if (length(current_values$woody500m) == current_values$patches &
      !any(is.na(current_values$woody500m)) &
      length(current_values$woody3000m) == current_values$patches &
      !any(is.na(current_values$woody3000m)) ){
      current_values$allpatchcomplete <- TRUE
    } else {
      current_values$allpatchcomplete <- FALSE
    }
  })
  
  output$nmimage <- renderImage({
    list(src = "./data/alaimgs_final/c4c37912-34ea-420b-9c77-65b59a8c9391.jpg",
    alt = "An image of noisy miners",
    height = "100px")
    }, deleteFile = FALSE, quoted = FALSE)
  
  current_values

  }
)
}
