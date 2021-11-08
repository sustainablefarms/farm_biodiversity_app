# patch attributes module
patchattr_UI <- function(id, attributes){
  inwoodlandtype <- if (is.null(attributes$IsRemnant)){
    character(0)
  } else if (attributes$IsRemnant){
    "remnant"
  } else {
    "planted"
  }
  innm <- if (is.null(attributes$noisy_miner)){
    character(0)
  } else if (attributes$noisy_miner){
    "yes"
  } else {
    "no"
  }
  ns <- NS(id)
  tagList(
    waiter::use_waiter(spinners = 1),
    #woodland type
    twocolumns(heading = "Type of woody coverage",
       left = tagList(
         radioButtons(ns("woodlandtype"),
                      label = "This woodland area is...",
                      choices = list(
                        "Remnant woodland" = "remnant",
                        "Planted woodland" = "planted"
                      ),
                      selected = inwoodlandtype
                      )
       ),
       right = tags$div(style = "background-color: #E6F0F0;",
          tags$h4("What is a Woodland Area?"),
          patchdefn,
          tags$div(tags$a("Learn more", href = "??")))),
    
    
   # noisy miners
   twocolumns(heading = "Presence of Noisy Miners",
              left = tagList(
                radioButtons(ns("nm"),
                             label = "Are there Noisy Miners in this area...",
                             choices = c("yes", "no"),
                             selected = innm)
                ),
              right = fluidRow(style = "background-color: #E6F0F0;",
column(9, 
  tags$h4("Why are Noisy Miners used in this modelling?"),
  tags$p("Noisy Miners are easy to recognise by their bright yellow eyes and beak.",
		"Noisy Miners are native, but typically have a detrimental effect on other small bird species.",
	  "This is because Noisy Miners are often aggressive towards other birds, preventing them from living in their patch."),
	tags$p("Noisy Miners dislike habitat with high amounts of midstorey (woody plants 2m-10m in height).",
		"You can discourage Noisy Miners by increasing the amount of midstorey in your patch, such as through underplanting with wattles, tea-trees, bottlebrushes, and other native shrubs."),
  tags$p("Visit",
    tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
           "BirdLife Australia"),
           "for a profile of Noisy Miners." 
    ),
  tags$div(tags$a("Learn more", href = "??"))
  ),
column(3, 
	  tags$div(tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
		           tags$img(src = "lowres-cb59057b0d4ef2a9ce75c19a128ca2ca.jpg",
		                    alt = "Noisy Miner photo",
		                    width = "100%"))),
		tags$a(href = "https://birdlifephotography.org.au/index.php/show-image?return=search&single&id=19910",
		       class = "datalabels",
		       HTML("&copy;Con Boekel 2016 BirdLife Photography"))
		)
)
),
	
   
  # WCF
twocolumns(heading = "Woody cover amounts",
  left = tagList(
    tags$p("Species occupancies depend heavily on the amount of 2m+ woody vegetation cover, or foliage cover,",
    "inside the patch and in the surrounding landscape."),
    infotext("Click to place pin on the map or enter Latitude and Longitude",
             "to load woody cover amounts for this woodland area.",
             "(see the ANU Centre for Water and Landscape Dynamics",
               tags$a(href = 'http://anuwald.science/tree', "Tree Change portal"), 
             "and ", tags$a(href = "https://doi.org/10.1016/j.jag.2020.102209",
                               "Liao et al. (IJAEOG, 2020)"), ")"),
  ),
  right = tagList(fluidRow(
    column(4, 
      tags$h4("Select location"),
      uiOutput(ns("latlonerror_short")),
      textInput(ns("lon"), "Longitude", value = attributes$usedlon, width = '100%',
                placeholder = ""),
      textInput(ns("lat"), "Latitude", value = attributes$usedlat, width = '100%',
                placeholder = ""),
      numericInput(ns("yearforcanopy"), "Representative year", 
                value = attributes$usedyear, 
                width = '100%',
                step = 1,
                min = 1990,
                max = 2019),
      actionButton(ns("getwoodycanopy"), "Load woody cover", class = "btn-primary")
    ),
    column(8, leaflet_UI(ns("leaflet"))),
  tags$div(style = "color:red; font-style:italic;", textOutput(ns("latlonerror"), inline = TRUE))
  )
  )),

#################################
	tags$div(HTML("<h4>Manually Set or Modify</h4>")),
        # 500m WCF
        tags$div(
          tags$html(tags$span("Nearby Woody Cover: within 500m of patch centre (% area), including cover inside patch")),
          sliderInput(label = NULL,
            inputId = ns("pc_woody500m"),
            min = 2, max = 20, step = 0.5,
	    width = "100%",
            value = attributes$woody500m)
          ),
        tags$div(
          tags$html(tags$span("Regional Woody Cover: within 3km of patch centre (% area)")),
          sliderInput(label = NULL,
            inputId = ns("pc_woody3000m"),
            min = 2, max = 20, step = 0.5,
	    width = "100%",
            value = attributes$woody3000m)

))
}

patchattr_Server <- function(id, bbox, savebutton, cancelbutton){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns
      savedvals <- reactiveVal()
      
      # from lat lon work
      leafletout <- leaflet_Server("leaflet", bbox)
      observe({
        validate(need(leafletout(), ""))
        updateTextInput(inputId = "lon", value = leafletout()$lng)
        updateTextInput(inputId = "lat", value = leafletout()$lat)
      })
      wait <- waiter::Waiter$new(id = ns("getwoodycanopy"), 
                                 html = waiter::spin_wave(),
                                 color = "#178BCA")
      pc_woody500m_latlon <- reactiveVal(label = "woody500m from latlon")
      pc_woody3000m_latlon <- reactiveVal(label = "woody3000m from latlon")
      usedlon <- reactiveVal(NULL, label = "Longitude used for getting canopy")
      usedlat <- reactiveVal(NULL, label = "Latitude used for getting canopy")
      usedyear <- reactiveVal(NULL, label = "Year used for getting canopy")
      latlonerror <- reactiveVal("", label = "latlonerror")
      latlonerror_short <- reactiveVal("", label = "latlonerror_short")
      getwoodycanopy_d <- reactive({
        validate(need(input$getwoodycanopy > 0, "")) #0 is the initial value ignore setting at this value (so ignoreInit works later)
        input$getwoodycanopy
        wait$show()}) %>% debounce(1000) # to stop heaps of clicking doing things, but show waiter from first click
      observeEvent(getwoodycanopy_d(), {
        session$sendCustomMessage("getwoodycanopyfromlatlon", "nothing")
        latlonerror("")
        latlonerror_short("")
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
            latlonerror_short("Please select a location")
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
      output$latlonerror_short <- renderUI({
        validate(need(latlonerror_short(), ""))
        tags$div(class = "warntext clearfix float-start", latlonerror_short())
      })
      
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
          nm = input[["nm"]],
          woodlandtype = input[["woodlandtype"]],
          showmap = input[["showmap"]],
          usedlon = usedlon(),
          usedlat = usedlat(),
          usedyear = usedyear()
          )
        out
      })
      if (FALSE & isTRUE(getOption("shiny.testmode"))){
        observeEvent(specifiedvals(),
                     {
                       showNotification(paste(names(specifiedvals()), specifiedvals(),
                                              sep = " = ", collapse = ", "), duration = 3)
                     })
      }
      
      
      # saving and cancelling
      observeEvent(savebutton(),{ showNotification("patch saved")})
      observeEvent(cancelbutton(),{ showNotification("patch reset")})
      observeEvent(savebutton(), {
        savedvals(specifiedvals())
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
      observeEvent(cancelbutton(), {
        validate(need(savedvals(), ""))
        updateSliderInput(inputId = "pc_woody500m",
                          value = savedvals()$woody500m)
        updateSliderInput(inputId = "pc_woody3000m",
                          value = savedvals()$woody3000m)
        updateRadioButtons(inputId = "nm", selected = savedvals()$nm)
        updateRadioButtons(inputId = "woodlandtype", selected = savedvals()$woodlandtype)
        if (isTruthy(savedvals()$usedlon)){
          updateTextInput(inputId = "lon", value = savedvals()$usedlon)
          updateTextInput(inputId = "lat", value = savedvals()$usedlat)
          updateTextInput(inputId = "yearforcanopy", value = savedvals()$usedyear)
        }
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
      
      
      
      setBookmarkExclude(c("woodlandtype", 
                           "showmap",
                           "pc_woody3000m",
                           "pc_woody500m",
                           "getwoodycanopy",
                           "nm",
                           "yearforcanopy",
                           "getwoodycanopy_waiter_hidden",
                           "lon",
                           "lat"))
      
      # convert out info
      reactive({
        out <- savedvals()
        out$IsRemnant <- (out$woodlandtype == "remnant")
        out$woodlandtype <- NULL
        out$noisy_miner <- (out$nm == "yes")
        out$nm <- NULL
        out
      })
    })
  }



app_patchattr <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  attributes <- list(IsRemnant = TRUE, noisy_miner = FALSE, woody500m = 3.5, woody3000m = 8.2, showmap = FALSE)
  bbox <- reactive({bbox_allregions})
  savebutton <- reactive(NULL)
  cancelbutton <- reactive(NULL)
  shinyApp(    {fluidPage(
    tags$script("$(function () {
          $('[data-toggle=tooltip]').tooltip()
        })"),
    includeCSS("./www/base.css"),
    patchattr_UI("patchattr", attributes),
    theme = bslib::bs_theme(version = 5, "lumen"))
  },
           function(input, output, session){patchattr_Server("patchattr",
                                                             bbox,
                                                             savebutton,
                                                             cancelbutton)}
  )
}
