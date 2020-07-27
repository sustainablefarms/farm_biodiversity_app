# function to add a single title + selector buttons to the ui
add_reference_ui <- function(
  entry_number, # an index to record which entry these data are linked to
  ui_selector # i.e. where in the UI should this go? Starts with a #
){
  insertUI(
    selector = paste0("#", ui_selector),
    ui = div(
      list(
        actionButton2(
          inputId = paste0("patch_number_", entry_number),
          label = paste0("Patch #", entry_number),
          class = "patch_badge"
        )
      )
    )
  )
}