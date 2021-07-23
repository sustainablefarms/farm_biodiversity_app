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
      maxpatchnum <- 6
      # set up reactive values
      # store patch attributes
      defaultnewpatchvalues <- list(woody500m = NA,
                                    woody3000m = NA,
                                    noisy_miner = NA,
                                    IsRemnant = NA,
                                    fromlatlon = FALSE)
      allpatchesdflt <- rep(list(defaultnewpatchvalues), maxpatchnum)
      names(allpatchesdflt) <- 1:maxpatchnum
      each_patch_attribute <- do.call(reactiveValues, args = allpatchesdflt) # list of patch attributes
      other_attributes <- reactiveValues(
        patchcomplete = c(FALSE, rep(NA, maxpatchnum - 1)),
        allpatchcomplete = FALSE,
        patches = 1,
        year = 2018
      )
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
      outinfo <- reactiveValues( #to send out of this module
        allpatchcomplete = FALSE,
        patches = 1,
        woody500m = NA,
        woody3000m = NA,
        noisy_miner = NA,
        IsRemnant = NA,
        year = NA
      )
      
  ## FARM
  
  # specify number of patches
  patchnum_Server("patch_selector", update, maxpatchnum = 6)

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
      other_attributes$patches <- update$numpatches_existing
      for (i in update$add_values){ # reactiveValues doesn't allow index extraction using multiple strings, hence this for loop grr
        each_patch_attribute[[as.character(i)]] <- defaultnewpatchvalues
      }
      other_attributes$patchcomplete[update$add_values] <- FALSE
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
      other_attributes$patches <- update$numpatches_existing
      for (i in update$remove_values){ #can't index multiple parts of a reactiveValues object at once, hence the for loop
        each_patch_attribute[[as.character(i)]] <- defaultnewpatchvalues
      }
      other_attributes$patchcomplete[update$remove_values] <- NA
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
          attributes = isolate(each_patch_attribute[[as.character(clicked_record$selected_patch)]]),
          ns
        )
      }
    }
    clicked_record$patch_buttons <- click_now$patches$value
  })
  out <- patchattr_Server("patchattr")

  # collect input values from modal
  observeEvent(input$choose_patch_attributes_execute, {
    each_patch_attribute[[as.character(clicked_record$selected_patch)]] <- out()
    # record as done internally
    other_attributes$patchcomplete[[clicked_record$selected_patch]] <- TRUE
    # now add a green tick
    output[[paste0("patch_num_complete_", clicked_record$selected_patch)]] <- renderUI({patchcompletesymbol})
    
    #update output values
    # check if the new saved values means all patches are complete
    if (all(isTRUE(other_attributes$patchcomplete[1:other_attributes$patches]))){
      outinfo$allpatchcomplete <- TRUE
    } else {
      outinfo$allpatchcomplete <- FALSE
    }
    each_patch_attribute_l <- reactiveValuesToList(each_patch_attribute)[as.character(1:other_attributes$patches)] #the order of the reactiveValues after listing is not fixed!
    outinfo$woody500m = vapply(each_patch_attribute_l, function(x) x[["woody500m"]], FUN.VALUE = 3.5)
    outinfo$woody3000m = vapply(each_patch_attribute_l, function(x) x[["woody3000m"]], FUN.VALUE = 3.5)
    outinfo$noisy_miner = vapply(each_patch_attribute_l, function(x) x[["noisy_miner"]], FUN.VALUE  = 0)
    outinfo$IsRemnant = vapply(each_patch_attribute_l, function(x) x[["IsRemnant"]], FUN.VALUE = 0)
    outinfo$year = other_attributes$year
    outinfo$patches = other_attributes$patches
    
    # close modal
    removeModal()
  })
  
  # BELOW IS OLD: update export info whenever patches become complete (or new incomplete patches added)
  # but doesn't work for editing the current patch
  # observeEvent({other_attributes$patchcomplete}, {
  # 
  # })
  
  outinfo #return value of the server
  }
)
}