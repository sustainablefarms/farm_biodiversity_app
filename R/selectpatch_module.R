selectpatchUI <- function(id){
  ns <- NS(id)
  tagList(
      HTML("<div class='subheader'><h2>PATCHES ON FARM</h2></div>"),
      verbatimTextOutput("text"),
      column(width = 3,
        patchnum_UI(ns("patch_selector")),
      ),
      column(width = 1),
      column(width = 8,
        actionButton2(
          inputId = ns("patch_number_1"),
          label = tags$span("Patch #1",
			    HTML("&ensp;"),
	     htmlOutput(outputId = ns("patch_num_complete_1"), inline = TRUE)),
          class = "patch_badge"),
        div(id = "placeholder")
      ))
}

selectpatchServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      # set up reactive values
      patch_attributes <- reactiveValues( # patch attributes
        allpatchcomplete = FALSE,
        patches = 1,
        woody500m = NA,
        woody3000m = NA,
        noisy_miner = NA,
        IsRemnant = NA,
        year = 2018)
      clicked_record <- reactiveValues( #record of
        #patches = 1, # number of patches - obsolete as of patchnumselector_module
        patch_buttons = c(0), #and number of times their buttons pressed
        selected_patch = NULL) #and the most recently clicked patch (for updating values)
      update <- reactiveValues( # for altering number of patch buttons
        numpatches_existing = 1, #Number of Patches Before Updating
        numpatches_new = 1, #Number of Patches Requested Just Now
        add_logical = FALSE,
        add_values = NULL,
        remove_logical = FALSE,
        remove_values = NULL)
      click_now <- reactiveValues(
        patches = NULL)
      
  ## FARM
  
  # specify number of patches
  patchnum_Server("patch_selector", update)
  observe(print(format(reactiveValuesToList(update))))

  ## PATCH BUTTONS
  # warning sign for patch number 1
  output[["patch_num_complete_1"]] <- renderUI({patchincompletewarn})

  # add 'patch' buttons
  observeEvent(update$add_values, {
    if(update$add_logical){
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
		patchincompletewarn})
      })
      update$add_logical <- FALSE
      update$numpatches_existing <- update$numpatches_new
      patch_attributes$patches <- update$numpatches_existing
      patch_attributes$woody500m[update$add_values] <- NA
      patch_attributes$woody3000m[update$add_values] <- NA
      patch_attributes$noisy_miner[update$add_values] <- NA
      patch_attributes$IsRemnant[update$add_values] <- NA
      patch_attributes$allpatchcomplete <- FALSE
    }
  })

  # substract 'patch' buttons
  observeEvent(update$remove_values, {
    if(update$remove_logical){
      lapply(update$remove_values, function(a){
        removeUI(
          selector = paste0("#patch_number_", a)  #the '#' here tells jQuery to find the UI element based on element id.
	  )
	removeUI(
          selector = paste0("#patch_num_complete_", a)  #the '#' here tells jQuery to find the UI element based on element id.
        )
      })
      update$remove_logical <- FALSE
      update$numpatches_existing <- update$numpatches_new
      patch_attributes$patches <- update$numpatches_existing
      patch_attributes$woody500m <- patch_attributes$woody500m[-update$remove_values]
      patch_attributes$woody3000m <- patch_attributes$woody3000m[-update$remove_values]
      patch_attributes$noisy_miner <- patch_attributes$noisy_miner[-update$remove_values]
      patch_attributes$IsRemnant <- patch_attributes$IsRemnant[-update$remove_values]
    }
  })

  # for each patch, launch a modal to set new values
  observe({
    click_now$patches <- input_tracker(
      input = input,
      string = "patch_number_[[:digit:]]+"
    )
    if(nrow(click_now$patches) == length(clicked_record$patch_buttons)){
      update_check <- (click_now$patches$value > clicked_record$patch_buttons)
      #For above: I suspect clicked_record$patch_buttons records the number of times a click has been executed, 
      #and click_now$patches$value increments each time the button is clicked. 
      #So value > patch_button means the modal needs to be opened.
      if(any(update_check)){
        clicked_record$selected_patch <- click_now$patches$id[which(update_check)]
        patch_modal(
          value = clicked_record$selected_patch,
          woody500m = patch_attributes$woody500m[clicked_record$selected_patch],
          woody3000m = patch_attributes$woody3000m[clicked_record$selected_patch],
          noisy_miner = patch_attributes$noisy_miner[clicked_record$selected_patch],
          IsRemnant = patch_attributes$IsRemnant[clicked_record$selected_patch],
          ns
        )
      }
    }
    clicked_record$patch_buttons <- click_now$patches$value
  })
  out <- patchattr_Server("patchattr")

  # collect input values from modal
  observeEvent(input$choose_patch_attributes_execute, {
    patch_attributes$woody500m[clicked_record$selected_patch] <- out()[["woody500m"]] 
    patch_attributes$woody3000m[clicked_record$selected_patch] <- out()[["woody3000m"]] 
    patch_attributes$noisy_miner[clicked_record$selected_patch] <- out()[["noisy_miner"]] 
    patch_attributes$IsRemnant[clicked_record$selected_patch] <- out()[["IsRemnant"]] 

    # add a green tick
    output[[paste0("patch_num_complete_", clicked_record$selected_patch)]] <- renderUI({patchcompletesymbol})

    removeModal()

    
    if (length(patch_attributes$woody500m) == patch_attributes$patches &
      !any(is.na(patch_attributes$woody500m)) &
      length(patch_attributes$woody3000m) == patch_attributes$patches &
      !any(is.na(patch_attributes$woody3000m)) ){
      patch_attributes$allpatchcomplete <- TRUE
    } else {
      patch_attributes$allpatchcomplete <- FALSE
    }
  })
  
  output$nmimage <- renderImage({
    list(src = "./data/alaimgs_final/c4c37912-34ea-420b-9c77-65b59a8c9391.jpg",
    alt = "An image of noisy miners",
    height = "100px")
    }, deleteFile = FALSE, quoted = FALSE)
  
  patch_attributes

  }
)
}
