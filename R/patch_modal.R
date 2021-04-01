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
      tags$div(
        class = "form-group shiny-input-container",
        tags$span(
          class = "checkbox",  #for some reason this creates an object that ends with a new line
          style = 'display:inline',
          tags$label(style='display:inline',tags$input(
            id = ns(paste0("noisy_miner_", value)),
            type = "checkbox",
            checked = if (noisy_miner){TRUE} else {NULL},
            tags$span("Noisy Miners present?")
          ))
        ),
        tags$span(class = "glyphicon glyphicon-info-sign",
                  `data-toggle` = "popover",
                  title = "Noisy Miners",
                  `data-trigger` = "click", #focus creates errors here
                  `data-html` = TRUE,
                  `data-content` =  
            tags$html(tags$div(
              style="text-align: center",
                tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
                   tags$img(src="https://images.ala.org.au/image/proxyImageThumbnailLarge?imageId=37e23d13-9cd2-465f-ab87-9a79b641752a",
                            alt="Noisy Miner Photo",
                            width=200,
                            height=200))),
                 tags$div(
                 "Noisy Miners noisily defend their ‘patch’ of trees from other birds. ...",
                 #, especially other species of honeyeaters which may be seen as competitors for the food resources, and these are vigorously chased away.",
                 # "Many other small birds are also driven from the area, and sometimes miners will even chase after cormorants or herons that may fly past, or harass them mercilessly if they perch somewhere in the miners’ territory.",
                 "Because of this aggressive behaviour, areas inhabited by Noisy Miners often support few other birds.",
                 "Image and text from",
                 tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
                        "BirdLife Australia"))
                    ))),
      actionButton(inputId = ns("choose_patch_attributes_execute"), label = "Save"),
      modalButton("Cancel"),
    title = paste0("Select attributes for patch #", value),
    footer = NULL,
    easyClose = FALSE
  )
)
}
