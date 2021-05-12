patch_modal <- function(
  value = 1,
  woody500m = NULL,
  woody3000m = NULL,
  noisy_miner = NULL,
  IsRemnant = NULL,
  ns #the namespace function
){
  if(is.null(woody500m) | is.na(woody500m)){woody500m <- 7}
  if(is.null(woody3000m) | is.na(woody3000m)){woody3000m <- 7}
  if(is.null(noisy_miner) | is.na(noisy_miner)){noisy_miner <- TRUE}
  if(is.null(IsRemnant) | is.na(IsRemnant)){IsRemnant <- TRUE}
  showModal(
    modalDialog(
      # the following enables bootstrap 3's inbuilt tooltips
      tags$script("$(function () {
          $('[data-toggle=tooltip]').tooltip()
        })"
      ),
      # the following enables bootstrap 3's inbuilt popovers
      tags$script("$(function () {
        $('[data-toggle=popover]').popover()
      })"),
        # 500m WCF
        tags$div(
          tags$html(tags$span("Woody vegetation canopy within 500m of patch centre (% area)"),
                    infotooltip(title = tags$div("This is the area of woody vegetation canopy, measured as a proportion of the total land area within 500m.",
                                              "It changes each year as the trees react to the prevailing weather.",
                                              "The percentage area of woody vegetation canopy can be obtained from",
                                              linknewtab(href = 'http://anuwald.science/tree',
                                                         "http://anuwald.science/tree"),
                                              "for any Australian location and any year since 1990.",
                                              HTML("<br><br>The values available for selection were chosen to cover 90% of our training data.")))
                    ),
          sliderInput(label = NULL,
            inputId = ns(paste0("pc_woody500m_", value)),
            min = 2, max = 20, step = 2,
            value = woody500m)
          ),
        tags$div(
          tags$html(tags$span("Woody vegetation canopy within 3km of patch centre (% area)"),
                    infotooltip(title = tags$div("This is the area of woody vegetation canopy, measured as a proportion of the total land area within 500m.",
                                              "It changes each year as the trees react to the prevailing weather.",
                                              "The percentage area of woody vegetation canopy can be obtained from",
                                              linknewtab(href = 'http://anuwald.science/tree',
                                                         "http://anuwald.science/tree"),
                                              "for any Australian location and any year since 1990.",
                                              HTML("<br><br>The values available for selection were chosen to cover 90% of our training data.")),
                                placement = "auto bottom")
                    ),
          sliderInput(label = NULL,
            inputId = ns(paste0("pc_woody3000m_", value)),
            min = 2, max = 20, step = 2,
            value = woody3000m)
          ),
      tags$div(
        inlinecheckBoxInput(ns(paste0("IsRemnant_", value)),
            value = if (IsRemnant){TRUE} else {NULL},
            label = tags$span("Is this patch remnant woodland?")
          )
        ),
      tags$br(),
      tags$div(
        inlinecheckBoxInput(ns(paste0("noisy_miner_", value)),
                            value = if (noisy_miner){TRUE} else {NULL},
                            tags$span("Noisy Miners present?")
        ),
        infopopover(title = "Noisy Miners", content = 
            tags$html(tags$div(
              style="text-align: center",
                linknewtab(href="https://birdlife.org.au/bird-profile/noisy-miner",
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
                 linknewtab(href="https://birdlife.org.au/bird-profile/noisy-miner",
                        "BirdLife Australia"))
                    )),
          `data-style` = "width = 100px;"
        ),
      tags$br(),
      actionButton(inputId = ns("choose_patch_attributes_execute"), label = "Save"),
      modalButton("Cancel"),
    title = paste0("Select attributes for patch #", value),
    footer = NULL,
    easyClose = FALSE
  )
)
}
