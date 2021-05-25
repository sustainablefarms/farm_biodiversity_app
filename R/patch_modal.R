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
        # 500m WCF
        tags$div(
          tags$html(tags$span("Nearby Woody Canopy: Woody vegetation canopy within 500m of patch centre (% area)"),
                    infotooltip(title = tags$div("This is the area of woody vegetation canopy, measured as a proportion of the total land area within 500m of the patch centre.",
"Tree canopy inside the patch is included.",
                                              "It can change over time in response to rainfall, disturbances such as fire, and management (such as planting more trees).",
                                              "The percentage area of woody vegetation canopy can be obtained from",
                                              linknewtab(href = 'http://anuwald.science/tree',
                                                         "http://anuwald.science/tree"),
                                              "for any Australian location and any year since 1990.",
                                              HTML("<br><br>The values available for selection were chosen to cover 90% of our training data.")))
                    ),
          sliderInput(label = NULL,
            inputId = ns(paste0("pc_woody500m_", value)),
            min = 2, max = 20, step = 0.5,
	    width = "100%",
            value = woody500m)
          ),
        tags$div(
          tags$html(tags$span("Regional Woody Canopy: Woody vegetation canopy within 3km of patch centre (% area)"),
                    infotooltip(title = tags$div("This is the area of woody vegetation canopy, measured as a proportion of the total land area within 3km of the patch centre.",
"Tree canopy inside the patch is included (but would have little effect due to the 3km scale).",
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
            min = 2, max = 20, step = 0.5,
	    width = "100%",
            value = woody3000m)
          ),
      tags$div(
        inlinecheckBoxInput(ns(paste0("IsRemnant_", value)),
            value = if (IsRemnant){TRUE} else {NULL},
            label = tags$span("Is this patch remnant woodland?")
          )
        ),
      tags$br(),
      fluidRow(
        column(5,
          inlinecheckBoxInput(ns(paste0("noisy_miner_", value)),
                              value = if (noisy_miner){TRUE} else {NULL},
                              tags$span("Noisy Miners present?")
          ),
          infotooltip(
            html = TRUE,
            title = tags$html(
                "Noisy Miners are easy to recognise by their bright yellow eyes and beak.",
		"Noisy Miners are native, but typically have a detrimental effect on other small bird species.",
	        "This is because Noisy Miners are often aggressive towards other birds, preventing them from living in their patch.",

		tags$br(), tags$br(),
		"Noisy Miners dislike habitat with high amounts of midstorey (woody plants 2m-10m in height).",
		"You can discourage Noisy Miners by increasing the amount of midstorey in your patch, such as through underplanting with wattles, tea-trees, bottlebrushes, and other native shrubs.",
                tags$br(),
	       	tags$br(),
                "Visit",
               linknewtab(href="https://birdlife.org.au/bird-profile/noisy-miner",
                      "BirdLife Australia"),
               "for a profile of Noisy Miners." 
              )
            )
          ),
        column(7, 
          linknewtab(href="https://birdlife.org.au/bird-profile/noisy-miner",
                    style = "float: left",
                    imageOutput(ns("nmimage"), height = "100px", inline = TRUE)),
          style = "font-size: 70%",
          linknewtab(href = "https://images.ala.org.au/image/details?imageId=c4c37912-34ea-420b-9c77-65b59a8c9391", "Creator: Joe"),
          tags$br(),
          linknewtab(href="https://creativecommons.org/licenses/by-nc-sa/3.0/",
                     tags$img(src = "https://licensebuttons.net/l/by-nc-sa/3.0/88x31.png",
                              alt = "CC BY-NC-SA 3.0",
                              height = "20px")
          )
        )
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
