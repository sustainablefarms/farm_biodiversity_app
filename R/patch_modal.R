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
      tipify(sliderInput(
        inputId = paste0("pc_woody_veg_", value),
        label = "Woody vegetation canopy within 500m of patch centre (% area)",
        min = 2, max = 20, step = 2,
        value = woody_veg),
        "The amount of woody vegetation canopy within 500m of the patch centre, as a percentage of the area within 500m."
        # "The percentage of area within 500m of the patch centre covered by woody vegetation canopy."
        ),
      popify(sliderInput(
        inputId = paste0("pc_midstorey_", value),
        label = "Vegetation between 2m and 10m high (% area of patch)",
        min = 0, max = 10, step = 1,
        value = midstorey),
        title = "",
        content = "The percentage of the patch that has vegetation between 2m and 10m tall (e.g regrowth eucalypt or acacia). Here are examples of what this looks like (insert insert...).<br> If you're interested in greater robustness, our ecologists estimated this quantity using 2x 50m point-intersect transects.",
        placement = "top",
        options = list(html = "true",
                       viewport = "viewport")),
      popify(checkboxInput(
        inputId = paste0("noisy_miner_", value),
        label = "Noisy Miners present?",
        value = noisy_miner),
        title = "",
        content = 'Noisy miners are territorial, loud, and easily detected. <a href="https://bie.ala.org.au/species/urn:lsid:biodiversity.org.au:afd.taxon:88df188e-fbc8-4220-82c1-d2fc03f2f83a#">Photos</a>. They are less likely to be present in patches with high midstorey.',
        trigger = "focus",
        options = list(html = "true")
      ),
      actionButton(inputId = "choose_patch_attributes_execute", label = "Save"),
      modalButton("Cancel"),
    title = paste0("Select attributes for patch #", value),
    footer = NULL,
    easyClose = FALSE
  )
)
}
