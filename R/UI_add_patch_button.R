# function to add a single title + selector buttons to the ui
add_patch_button <- function(
  entry_number, # an index to record which entry these data are linked to
  ui_selector, # i.e. where in the UI should this go? Starts with a #
  ns # shiny name space function
){
  insertUI(
    selector = paste0("#", ui_selector),
    ui = div(
      id = paste0("patch_number_", entry_number),
      actionButton_notdfl2(
        inputId = ns(paste0("patch_number_", entry_number)),
        label = tags$span(paste0("Patch #", entry_number),
			    HTML("&ensp;"),
	     htmlOutput(outputId = ns(paste0("patch_num_complete_", entry_number)), inline = TRUE)),
        class = "patch_badge"
      ),
    )
  )
}
