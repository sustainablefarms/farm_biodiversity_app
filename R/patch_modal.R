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
      sliderInput(
        inputId = ns(paste0("pc_woody_veg_", value)),
        label = "Woody vegetation canopy within 500m of patch centre (% area)",
        min = 2, max = 20, step = 2,
        value = woody_veg) %>%
        tippy::with_tippy(#
                          paste('<div style="text-align: left">A patch is a region of woodland vegetation that is',
                                '<br>(1) 1ha - 10ha (TBC) in area, ',
                                '<br>(2) has similar vegetation structure throughout, and',
                                '<br>(3) is approximately 50m from other woody vegetation.',
                                '<br><br> Currently the patch must be remnant box gum grassy woodlands.</div>'),
                          arrow = TRUE,
                          interactive = TRUE,
                          allowHTML = "true"),
        # tippy::with_tippy(
        # "The amount of woody vegetation canopy within 500m of the patch centre, as a percentage of the area within 500m.",
        # zIndex = 9999),
        # "The percentage of area within 500m of the patch centre covered by woody vegetation canopy."
        # options = list(html = "true",
        #                viewport = "viewport")),
      sliderInput(
        inputId = ns(paste0("pc_midstorey_", value)),
        label = "Midstorey vegetation (2m - 10m tall) within patch (% area of patch)",
        min = 0, max = 10, step = 1,
        value = midstorey) %>%
        shinyBS::tipify(
        "The percentage of the patch that has vegetation between 2m and 10m tall (e.g regrowth eucalypt or acacia). Our ecologists estimated this quantity using 2x 50m point-intersect transects.",
        placement = "top",
        options = list(html = "true",
                       viewport = "viewport")),
      shinyBS::tipify(checkboxInput(
        inputId = ns(paste0("noisy_miner_", value)),
        label = "Noisy Miners present?",
        value = noisy_miner),
        'Noisy miners are territorial, loud, and easily detected. <a href="https://bie.ala.org.au/species/urn:lsid:biodiversity.org.au:afd.taxon:88df188e-fbc8-4220-82c1-d2fc03f2f83a#">Photos</a>. They are less likely to be present in patches with high midstorey.',
        trigger = "hover focus",
        options = list(html = "true")
      ),
      actionButton(inputId = ns("choose_patch_attributes_execute"), label = "Save"),
      modalButton("Cancel"),
    title = paste0("Select attributes for patch #", value),
    footer = NULL,
    easyClose = FALSE
  )
)
}
