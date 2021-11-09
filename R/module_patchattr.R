# patch attributes module
patchattr_UI <- function(id, pid, attributes){
  ns <- NS(id)
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
  internals <- tagList(
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
          tags$h3("What can my Woodland Area be?"),
          tags$p("The Woodland Area must be approximately 1ha -10ha in area",
                 "with similar vegetation structure throughout.",
                 "Here, remnant woodland is Box Gum Grassy Woodland that has survived since before colonisation.",
                 "Planted woodland is assumed to be 3+ years old, eucalypt-dominated,",
                 "established by tubestock or direct seeding, and fenced at time of planting."),
          tags$p(tags$a("Learn more", href = "??"), tags$em("Do we have something suitable on the SF website?"))
       )
    ),
    
    
   # noisy miners
   twocolumns(heading = "Presence of Noisy Miners",
              left = tagList(
                radioButtons(ns("nm"),
                             label = "Are there Noisy Miners in this area...",
                             choices = c("yes", "no"),
                             selected = innm)
                ),
              right = fluidRow(style = "background-color: #E6F0F0;",
column(12, 
  tags$h4("Why are Noisy Miners used in this modelling?"),
  tags$div(class = "clearfix",
  tags$div(class = "col-md-3 float-md-end mb-3 ms-md-3",
           tags$div(tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
                           tags$img(src = "lowres-cb59057b0d4ef2a9ce75c19a128ca2ca.jpg",
                                    alt = "Noisy Miner photo",
                                    width = "100%"))),
           tags$a(href = "https://birdlifephotography.org.au/index.php/show-image?return=search&single&id=19910",
                  class = "datalabels",
                  HTML("&copy;Con Boekel 2016 BirdLife Photography"))
  ),
  tags$p(
		"Noisy Miners are native, but are often aggressive towards other small bird species,
		preventing them from living in their patch."),
  tags$p(
    "Noisy Miners are easy to recognise by their bright yellow eyes and beak.",
    "Visit",
    tags$a(href="https://birdlife.org.au/bird-profile/noisy-miner",
           "BirdLife Australia"),
    "for a profile of Noisy Miners." 
    ),
	tags$p(
	  "You can discourage Noisy Miners by increasing the amount of midstorey in your patch, such as through underplanting with wattles, tea-trees, bottlebrushes, and other native shrubs.",
	  "This is because Noisy Miners dislike habitat with high amounts of midstorey (woody plants 2m-10m in height)."
		),

  tags$p(tags$a("Learn more", href = "??"), tags$em("Do we have something suitable on the SF website?"))
  )
)

)
),
	
   
  # WCF
twocolumns(heading = "Woody cover amounts",
  left = tagList(
    tags$p("Species occupancies depend heavily on the amount of 2m+ woody vegetation cover",
    "inside the woodland and in the surrounding landscape."),
    infotext("Click to place pin on the map, or enter Latitude and Longitude,",
             "then load woody vegetation cover amounts for this woodland area.",
             "Modify the amounts by dragging."
             ),
  ),
  right = tagList(fluidRow(
    column(4, 
      tags$h3("Select location"),
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
      tags$div(
        actionButton(ns("getwoodycanopy"), "Load woody cover", class = "btn-primary"),
        tags$span(id = ns("tickplace"),
            style = "width: 2rem; height: 2rem; background-color: #BBBBBB; vertical-align: middle;",
            style = "margin-left: 1rem; position: relative")
      )
    ),
    column(8, leaflet_UI(ns("leaflet"))),
  tags$div(class = "datalabels",
      "Woody cover estimates from the",
      tags$a("ANU Centre for Water and Landscape Dynamics.", href = "http://wald.anu.edu.au/"),
      "See",
      tags$a(href = 'http://anuwald.science/tree', "Tree Change portal"), 
      "and ", tags$a(href = "https://doi.org/10.1016/j.jag.2020.102209",
                     "Liao et al. (IJAEOG, 2020).")),
  tags$div(style = "color:red; font-style:italic;", textOutput(ns("latlonerror"), inline = TRUE))
  ),
  tags$h3("Modify amounts"),
  # 500m WCF
  tags$div(
    tags$html(tags$span("Nearby Woody Cover: within 500m of patch centre (% area), including cover inside patch")),
    sliderInput(label = NULL,
                inputId = ns("pc_woody500m"),
                min = 2, max = 20, step = 0.5,
                width = "100%",
                value = attributes$woody500m)
  ),
  #3000m WCF
  tags$div(
    tags$html(tags$span("Regional Woody Cover: within 3km of patch centre (% area)")),
    sliderInput(label = NULL,
                inputId = ns("pc_woody3000m"),
                min = 2, max = 20, step = 0.5,
                width = "100%",
                value = attributes$woody3000m)
  )
  )
))
  

acc_item <- accordion_item(title = paste("Woodland area", pid),
    id = ns("accitem"),
    internals,
    footer = tagList(
      actionButton(ns("delete"), "Delete woodland area", icon = icon("trash"),
                   class = "btn-danger"),
      do.call(actionButton,
        args = c(list(ns("cancel"), "Cancel", class = "btn-secondary"),
             toggle_attr(paste0(ns("accitem"), "_body"))
             )),
      do.call(actionButton,
              args = c(list(ns("save"), "Save and Close", class = "btn-primary"),
                       toggle_attr(paste0(ns("accitem"), "_body"))
              ))
      ),
    footerdflt = "none",
    opentype = "edit"
    )
inattrisdefault <- patchequalsdefault(attributes)
if (inattrisdefault){acc_item <- expanditem(acc_item)}
return(acc_item)
}

patchattr_Server <- function(id, pid, bbox){
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
      pc_woody500m_latlon <- reactiveVal(label = "woody500m from latlon")
      pc_woody3000m_latlon <- reactiveVal(label = "woody3000m from latlon")
      usedlon <- reactiveVal(NULL, label = "Longitude used for getting canopy")
      usedlat <- reactiveVal(NULL, label = "Latitude used for getting canopy")
      usedyear <- reactiveVal(NULL, label = "Year used for getting canopy")
      latlonerror <- reactiveVal("", label = "latlonerror")
      latlonerror_short <- reactiveVal("", label = "latlonerror_short")
      getwoodycanopy_d <- reactive({
        validate(need(input$getwoodycanopy > 0, "")) #0 is the initial value ignore setting at this value (so ignoreInit works later)
        removeUI(paste0("#", ns("tick")), immediate = TRUE, multiple = TRUE)
        removeUI(paste0("#", ns("tickspinner")), immediate = TRUE, multiple = TRUE) #means multiple spinners won't ever be created
        insertUI(paste0("#", ns("tickplace")),
                 where = "afterBegin",
                 ui = tags$div(id = ns("tickspinner"), class = "spinner-border", role="status",
                               style="width: 2rem; height: 2rem; position: absolute; color: #168BCB"),
                 immediate = TRUE)
        input$getwoodycanopy
        }) %>% debounce(5000) # to stop heaps of clicking doing things
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
        removeUI(paste0("#", ns("tickspinner")), immediate = TRUE, multiple = TRUE)
        if (!is.null(wcfs[["500m"]])){
              usedlon(lon); usedlat(lat); usedyear(year)
              updateSliderInput(inputId = "pc_woody500m",
                                value = wcfs$`500m`)
              updateSliderInput(inputId = "pc_woody3000m",
                                value = wcfs$`3000m`)
              pc_woody500m_latlon(wcfs[[1]])
              pc_woody3000m_latlon(wcfs[[2]])
              insertUI(paste0("#", ns("tickplace")),
                where = "afterBegin",
                ui = tags$span(id = ns("tick"), 
                              icon("check-circle", style = "font-size: 2rem; color: #168BCB; position: absolute;"),
                              style = "position: absolute;"),
                immediate = TRUE)
        }

      },
        ignoreInit = FALSE)
      
      output$latlonerror <- renderText(latlonerror())
      output$latlonerror_short <- renderUI({
        validate(need(latlonerror_short(), ""))
        tags$div(class = "clearfix",
          tags$div(class = "warntext float-start", latlonerror_short())
        )
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
          usedlon = usedlon(),
          usedlat = usedlat(),
          usedyear = usedyear(),
          delete = FALSE,
          pid = pid
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
      observeEvent(input$save,{ showNotification("patch saved")})
      observeEvent(input$cancel,{ showNotification("patch reset")})
      observeEvent(input$save, {
        savedvals(specifiedvals())
      }, ignoreNULL = FALSE, ignoreInit = FALSE)
      observeEvent(input$cancel, {
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
      
      # output a delete trigger
      observeEvent(input$delete, {
        print("Delete button clicked")
        savedvals(list("delete" = TRUE, pid = pid))
      }, ignoreInit = TRUE)
      
      
      setBookmarkExclude(c("woodlandtype", 
                           "pc_woody3000m",
                           "pc_woody500m",
                           "getwoodycanopy",
                           "nm",
                           "yearforcanopy",
                           "lon",
                           "lat"))
      
      # convert out info
      reactive({
        out <- savedvals()
        print("Computing out list")
        if (length(out$nm) == 0) { #includes NULL and character(0)
          noisy_miner <- NULL
        } else {
          noisy_miner <- switch(out$nm, "yes" = TRUE, "no" = FALSE, NULL) #NULL if not yes or no
        }
        if (length(out$woodlandtype) == 0) { #includes NULL and character(0)
          IsRemnant <- NULL
        } else {
          IsRemnant <- switch(out$woodlandtype, "remnant" = TRUE, "planted" = FALSE, NULL) #NULL if not yes or no
        }
        out$IsRemnant <- IsRemnant
        out$woodlandtype <- NULL
        out$noisy_miner <- noisy_miner
        out$nm <- NULL
        out
      })
    })
  }



app_patchattr <- function(){
  main_app_prep()
  enableBookmarking(store = "disable")
  attributes <- list(woody500m = 3.5, woody3000m = 8.2)
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
