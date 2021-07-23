patch_modal <- function(
  value = 1,
  attributes = NULL,
  ns #the namespace function
){
  if(is.null(attributes$woody500m) | is.na(attributes$woody500m)){attributes$woody500m <- round(new_data_mean$WCF_500/0.5) * 0.5}
  if(is.null(attributes$woody3000m) | is.na(attributes$woody3000m)){attributes$woody3000m <- round(new_data_mean$WCF_3000/0.5) * 0.5}
  if(is.null(attributes$noisy_miner) | is.na(attributes$noisy_miner)){attributes$noisy_miner <- TRUE}
  if(is.null(attributes$IsRemnant) | is.na(attributes$IsRemnant)){attributes$IsRemnant <- TRUE}
  showModal(
    modalDialog(
      # the following enables bootstrap 3's inbuilt tooltips
      tags$script("$(function () {
          $('[data-toggle=tooltip]').tooltip()
        })"
      ),
      patchattr_UI(ns("patchattr"), attributes),
      tags$br(),
      actionButton(inputId = ns("choose_patch_attributes_execute"), label = "Save"),
      modalButton("Cancel"),
    title = tags$span(paste0("Select attributes for patch #", value),
		      infotooltip(title = patchdefn,
				  placement = "bottom")),
    footer = NULL,
    easyClose = FALSE
  )
)
}
