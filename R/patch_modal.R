patch_modal <- function(
  value = 1,
  woody_veg = NULL,
  midstorey = NULL,
  noisy_miner = NULL
){
  if(is.null(woody_veg) | is.na(woody_veg)){woody_veg <- 8}
  if(is.null(midstorey) | is.na(midstorey)){midstorey <- 6}
  if(is.null(noisy_miner) | is.na(noisy_miner)){noisy_miner <- TRUE}
  showModal(
    modalDialog(
      sliderInput(
        inputId = paste0("pc_woody_veg_", value),
        label = "Amount of woody vegetation (%)",
        min = 2, max = 20, step = 2,
        value = woody_veg),
      sliderInput(
        inputId = paste0("pc_midstorey_", value),
        label = "Amount of midstorey cover (%)",
        min = 0, max = 10, step = 1,
        value = midstorey),
      checkboxInput(
        inputId = paste0("noisy_miner_", value),
        label = "Noisy Miners present?",
        value = noisy_miner),
      actionButton(inputId = "choose_patch_attributes_execute", label = "Save"),
      modalButton("Cancel"),
    title = paste0("Select attributes for patch #", value),
    footer = NULL,
    easyClose = FALSE
  )
)
}
