patch_modal <- function(
  value = 1,
  woody_veg = NULL,
  midstorey = NULL,
  noisy_miner = NULL,
  ns #the namespace function
){
  if(is.null(woody_veg) | is.na(woody_veg)){woody_veg <- 8}
  if(is.null(midstorey) | is.na(midstorey)){midstorey <- 6}
  if(is.null(noisy_miner) | is.na(noisy_miner)){noisy_miner <- TRUE}
  showModal(
    modalDialog(
      # enables popovers in this modal!! (this doesn't work in the module UI)
      tags$script("$(function () {
      $('[data-toggle=popover]').popover()
    })"),
      sliderInput(
        inputId = ns(paste0("pc_woody_veg_", value)),
        label = tags$html(tags$span("Woody vegetation canopy within 500m of patch centre (% area)",
                                    `data-toggle` = "tooltip",
                                    title = "The amount of woody vegetation canopy within 500m of the patch centre, as a percentage of the area within 500m.")),
        min = 2, max = 20, step = 2,
        value = woody_veg),
      sliderInput(
        inputId = ns(paste0("pc_midstorey_", value)),
        label = tags$html(tags$span('Midstorey vegetation (2m - 10m tall) within patch (% area of patch)'),
                          tags$button(type="button",
                                      class="btn btn-lg btn-danger",
                                      `data-toggle`="popover",
                                      title="Midstorey Definition Detail",
                                      `data-content`="Midstorey vegetation (2m - 10m tall) within patch (% area of patch)",
                                      "Click to toggle popover")),
        min = 0, max = 10, step = 1,
        value = midstorey),
      checkboxInput(
        inputId = ns(paste0("noisy_miner_", value)),
        label = tags$html(tags$span("Noisy Miners present?",
                                    `data-toggle` = "popover",
                                    title = "Noisy Miners",
                                    `data-html` = TRUE,
                                    `data-content` =  'Noisy miners are territorial, loud, and easily detected. <a href="https://bie.ala.org.au/species/urn:lsid:biodiversity.org.au:afd.taxon:88df188e-fbc8-4220-82c1-d2fc03f2f83a#">Photos</a>. They are less likely to be present in patches with high midstorey.')),
        value = noisy_miner),
      actionButton(inputId = ns("choose_patch_attributes_execute"), label = "Save"),
      modalButton("Cancel"),
    title = paste0("Select attributes for patch #", value),
    footer = NULL,
    easyClose = FALSE
  )
)
}
