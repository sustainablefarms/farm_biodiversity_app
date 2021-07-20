patch_modal <- function(
  value = 1,
  woody500m = NULL,
  woody3000m = NULL,
  noisy_miner = NULL,
  IsRemnant = NULL,
  ns #the namespace function
){
  if(is.null(woody500m) | is.na(woody500m)){woody500m <- round(new_data_mean$WCF_500/0.5) * 0.5}
  if(is.null(woody3000m) | is.na(woody3000m)){woody3000m <- round(new_data_mean$WCF_3000/0.5) * 0.5}
  if(is.null(noisy_miner) | is.na(noisy_miner)){noisy_miner <- TRUE}
  if(is.null(IsRemnant) | is.na(IsRemnant)){IsRemnant <- TRUE}
  showModal(
    modalDialog(
      # the following enables bootstrap 3's inbuilt tooltips
      tags$script("$(function () {
          $('[data-toggle=tooltip]').tooltip()
        })"
      ),
      patchattr_UI(ns("patch1"), value, woody500m, woody3000m, noisy_miner, IsRemnant),
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
