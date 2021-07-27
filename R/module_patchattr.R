# patch attributes module
patchattr_UI <- function(id, attributes){
  ns <- NS(id)
  tagList(
        waiter::use_waiter(spinners = 1),
   # remnant and noisy miners first
   fluidRow(
     column(7, 
      fluidRow(
        inlinecheckBoxInput(ns("IsRemnant"),
            value = if (attributes$IsRemnant){TRUE} else {NULL},
            label = tags$span("Is this patch remnant woodland?")
          ),
	infotooltip(
          html = TRUE,
	  title = tags$div("If the patch is", tags$em("not"), "a remnant, then",
			   "this app assumes that it is a", tags$em("planted patch"), 
	                  plantedpatchdefn))
			    
        ),
      fluidRow(inlinecheckBoxInput(ns("noisy_miner"),
                              value = if (attributes$noisy_miner){TRUE} else {NULL},
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
          )),
        column(5, 
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
	
	# WCF
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
            value = attributes$woody500m)
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
            value = attributes$woody3000m),

 # from lat lon
shinyWidgets::materialSwitch(ns("fromlatlon"),
                             label = tags$span("Get woody canopy amounts from satellite",
                                               "(see ", linknewtab(href = 'http://anuwald.science/tree',
                                                                   "http://anuwald.science/tree"), 
                                               "and ",linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
                                                               "Liao et al. (IJAEOG, 2020)"), ")"),
                             value = attributes$fromlatlon,
                             status = "primary",
                             width = '100%'),
tags$div(id = ns("inputfromlatlonpanel"),
          conditionalPanel("input.fromlatlon",
              fluidRow(
                column(4, textInput(ns("lon"), "Longitude", value = attributes$usedlon, width = '100%',
                                    placeholder = "145.123456789")),
                column(4, textInput(ns("lat"), "Latitude", value = attributes$usedlat, width = '100%',
                                    placeholder = "-35.123456789")),
                column(2, textInput(ns("yearforcanopy"), "Year", value = attributes$usedyear, width = '100%',
                                    placeholder = "2018")),
                column(2, actionButton(ns("getwoodycanopy"), "Get", class = "download_badge"))
              ),
              tags$div(style = "color:red; font-style:italic;", textOutput(ns("latlonerror"), inline = TRUE)),
                     # tags$div("test: ", textOutput(ns("pc_woody500m_latlon"))),
                     # tags$div("test: ", textOutput(ns("pc_woody3000m_latlon"))),
              ns = ns))
                     # tags$div("Satellite based Regional Woody Canopy Cover:",
                              # textOutput(ns("pc_woody3000m_latlon"), inline = TRUE))
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
      wait <- waiter::Waiter$new(id = ns("getwoodycanopy"), 
                                 html = waiter::spin_wave(),
                                 color = "#178BCA")
      pc_woody500m_latlon <- reactiveVal(label = "woody500m from latlon")
      pc_woody3000m_latlon <- reactiveVal(label = "woody3000m from latlon")
      usedlon <- reactiveVal(NULL, label = "Longitude used for getting canopy")
      usedlat <- reactiveVal(NULL, label = "Latitude used for getting canopy")
      usedyear <- reactiveVal(NULL, label = "Year used for getting canopy")
      latlonerror <- reactiveVal("", label = "latlonerror")
      getwoodycanopy_d <- reactive({
        validate(need(input$getwoodycanopy > 0, "")) #0 is the initial value ignore setting at this value (so ignoreInit works later)
        input$getwoodycanopy
        wait$show()}) %>% debounce(1000) # to stop heaps of clicking doing things, but show waiter from first click
      observeEvent(getwoodycanopy_d(), {
        latlonerror("")
        wcfs <- tryCatch(
          {
            lat <- parsechar(input$lat, "Latitude")
            lon <- parsechar(input$lon, "Longitude")
            year <- parsechar(input$yearforcanopy, "Year")
            wcfs <- canopyfromlatlon(lon,lat,year)
            checkfinalwcfs(wcfs)
            wcfs
          },
          error = function(e) {
            latlonerror(e$message)
            pc_woody500m_latlon(NULL)
            pc_woody3000m_latlon(NULL)
            return(e)
          },
          warning = function(w) {latlonerror(w$message); return(wcfs)}
        )            
        if (!is.null(wcfs[["500m"]])){
              usedlon(lon); usedlat(lat); usedyear(year)
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
        out <- list(woody500m = input[["pc_woody500m"]],
          woody3000m = input[["pc_woody3000m"]],
          noisy_miner = input[["noisy_miner"]],
          IsRemnant = input[["IsRemnant"]],
          fromlatlon = input[["fromlatlon"]],
          usedlon = usedlon(),
          usedlat = usedlat(),
          usedyear = usedyear()
          )
        out
      })
      if (isTRUE(getOption("shiny.testmode"))){
        observeEvent(specifiedvals(),
                     {
                       showNotification(paste(names(specifiedvals()), specifiedvals(),
                                              sep = " = ", collapse = ", "), duration = 0.5)
                     })
      }
      return(specifiedvals)
    })
  }



app_patchattr <- function(){
  attributes <- list(IsRemnant = TRUE, noisy_miner = FALSE, woody500m = 3.5, woody3000m = 8.2, fromlatlon = FALSE)
  shinyApp(    {fluidPage(
    includeCSS("./www/base.css"),
    patchattr_UI("patchattr", attributes),
    theme = bslib::bs_theme(version = 3, "lumen"))
  },
           function(input, output, session){patchattr_Server("patchattr")}
  )
}
