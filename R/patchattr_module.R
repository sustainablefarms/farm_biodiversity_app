# patch attributes module
patchattr_UI <- function(id, woody500m, woody3000m, noisy_miner, IsRemnant){
  ns <- NS(id)
  tagList(
        waiter::use_waiter(),
        # 500m WCF
        tags$div(
          tags$html(tags$span("Nearby Woody Canopy: Woody vegetation canopy within 500m of patch centre (% area)"),
                    infotooltip(title = tags$div("This is the area of woody vegetation canopy, measured as a proportion of the total land area within 500m of the patch centre.",
"Tree canopy inside the patch is included.",
                                         WCFdesc(),
                                            tags$p("The values available for selection were chosen to cover 90% of our data.")))
                    ),
          sliderInput(label = NULL,
            inputId = ns("pc_woody500m"),
            min = 2, max = 20, step = 0.5,
	    width = "100%",
            value = woody500m)
          ),
        tags$div(
          tags$html(tags$span("Regional Woody Canopy: Woody vegetation canopy within 3km of patch centre (% area)"),
                    infotooltip(title = tags$div("This is the area of woody vegetation canopy, measured as a proportion of the total land area within 3km of the patch centre.",
"Tree canopy inside the patch is included (but would have little effect due to the 3km scale).",
               WCFdesc(),
               tags$p("The values available for selection were chosen to cover 90% of our data.")),
                                placement = "auto bottom")
                    ),
          sliderInput(label = NULL,
            inputId = ns("pc_woody3000m"),
            min = 2, max = 20, step = 0.5,
	    width = "100%",
            value = woody3000m),

tags$div(inlinecheckBoxInput(ns("fromlatlon"),
                    value = FALSE,
                    tags$span("Get woody canopy from latlon")
)),
          conditionalPanel("input.fromlatlon",
                     textInput(ns("lat"), "Latitude", value = "", width = '100px',
                               placeholder = "-36.123456789"),
                     textInput(ns("lon"), "Longitude", value = "", width = '100px',
                               placeholder = "longitude"),
                     textInput(ns("yearforcanopy"), "Year", value = "2018", width = '100px',
                               placeholder = "2018"),
                     actionButton(ns("getwoodycanopy"), "Get", class = "download_badge"),
                     tags$div(style = "color:red; font-style:italic;", textOutput(ns("latlonerror"), inline = TRUE)),
                     # tags$div("test: ", textOutput(ns("pc_woody500m_latlon"))),
                     # tags$div("test: ", textOutput(ns("pc_woody3000m_latlon"))),
                     ns = ns)
                     # tags$div("Satellite based Regional Woody Canopy Cover:",
                              # textOutput(ns("pc_woody3000m_latlon"), inline = TRUE))
                   ),
   
      tags$div(
        inlinecheckBoxInput(ns("IsRemnant"),
            value = if (IsRemnant){TRUE} else {NULL},
            label = tags$span("Is this patch remnant woodland?")
          ),
	infotooltip(
          html = TRUE,
	  title = tags$div("If the patch is", tags$em("not"), "a remnant, then",
			   "this app assumes that it is a", tags$em("planted patch"), 
	                  plantedpatchdefn))
			    
        ),
      tags$br(),
      fluidRow(
        column(5,
          inlinecheckBoxInput(ns("noisy_miner"),
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
          ), 
          tags$div("shortspecvalues", textOutput(ns("text")))
        )
      )
)
}

patchattr_Server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      print(input)
      ns <- session$ns
      # noisy miner image
      output$nmimage <- renderImage({
        list(src = "./data/alaimgs_final/c4c37912-34ea-420b-9c77-65b59a8c9391.jpg",
        alt = "An image of noisy miners",
        height = "100px")
      }, deleteFile = FALSE, quoted = FALSE)

      
      # from lat lon work
      wait <- waiter::Waiter$new(id = ns("getwoodycanopy"))
      pc_woody500m_latlon <- reactiveVal(label = "woody500m from latlon")
      pc_woody3000m_latlon <- reactiveVal(label = "woody3000m from latlon")
      latlonerror <- reactiveVal("", label = "latlonerror")
      observeEvent(input$getwoodycanopy, {
        wait$show()
        latlonerror("")
        lat <- suppressWarnings(as.numeric(input$lat))
        lon <- suppressWarnings(as.numeric(input$lon))
        year <- suppressWarnings(as.numeric(input$yearforcanopy))
        if (any(is.na(lat), is.na(lon), is.na(year))){
          latlonerror("One or more inputs could not be interpreted as numerical.")
          pc_woody500m_latlon(NULL)
          pc_woody3000m_latlon(NULL)
        }  
        wcfs <- canopyfromlatlon(lon,lat,year)
        if (!is.null(wcfs)){
          if (any(wcfs < 2) | any(wcfs > 20)){
            latlonerror(
              paste(sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m and %3.1f%% of the area within 3km.", wcfs[[1]], wcfs[[2]]),
                    "At least one of these percentages is outside the capabilities of the model.",
                    "Treat any estimates of bird occupancy with an extra degree of caution.")
              )
          } 
          updateSliderInput(inputId = "pc_woody500m",
                            value = wcfs$`500m`)
          updateSliderInput(inputId = "pc_woody3000m",
                            value = wcfs$`3000m`)
          pc_woody500m_latlon(wcfs[[1]])
          pc_woody3000m_latlon(wcfs[[2]])
        }
        wait$hide()
      },
        ignoreInit = FALSE)
      
      output$latlonerror <- renderText(latlonerror())
      output$pc_woody500m_latlon <- renderText({
        validate(need(pc_woody500m_latlon, label = "woody500m"))
        pc_woody500m_latlon()})
      output$pc_woody3000m_latlon <- renderText({
        validate(need(pc_woody3000m_latlon, label = "woody3000m"))
        pc_woody3000m_latlon()})
    
      # combine values into an output
      specifiedvals <- reactive({
        out <- c(woody500m = input[["pc_woody500m"]],
          woody3000m = input[["pc_woody3000m"]],
          noisy_miner = input[["noisy_miner"]],
          IsRemnant = input[["IsRemnant"]],
          fromlatlon = input[["fromlatlon"]]
          )
        out
      })
      output$text <- renderText(format(specifiedvals()))
      return(specifiedvals)
    })
  }