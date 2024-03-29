# patch attributes module
patchattr_UI <- function(ns, pid, attributes, patchcomplete = TRUE){ #ns rather than id because don't want to add namespace since generating withing Server
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
    twocolumns(heading = tags$div(id = ns("wtheading"), "Type of woodland"),
       left = tagList(
         radioButtons(ns("woodlandtype"),
                      label = "This woodland area is...",
                      choices = list(
                        "remnant woodland" = "remnant",
                        "planted woodland" = "planted"
                      ),
                      selected = inwoodlandtype
                      )
       ),
       right = tags$div(class = "p-3", style = paste("background-color:", appcolors[["Green 10"]], ";"),
          tags$p(appname,
		 "requires", tags$b("woodland areas"), "that are between approximately 1 and 10 hectares in size",
                 "with similar vegetation structure throughout.",
                 tags$b("Remnant woodland"), "is Box Gum Grassy Woodland that has never been cleared.",
                 tags$b("Planted woodland"), "is assumed to be eucalypt-dominated,",
                 "established at least three years ago by planting tubestock or by direct seeding, and fenced at the time of planting.")
       )
    ),
    
    
   # noisy miners
   twocolumns(heading = tags$div(id = ns("nmheading"), "Presence of Noisy Miners"),
              left = tagList(
                radioButtons(ns("nm"),
                             label = "Are there Noisy Miners in this area?",
                             choices = c("yes", "no"),
                             selected = innm)
                ),
       right = tags$div(class = "p-3", style = paste("background-color:", appcolors[["Green 10"]], ";"),
  #  tags$h3("Why Noisy Miners?", style = paste("color:", appcolors[["Dark Green"]], ";")),
  tags$div(class = "clearfix",
  tags$div(class = "col-md-3 float-md-end mb-3 ms-md-3",
           tags$div(linknewtab(href="https://birdlife.org.au/bird-profile/noisy-miner",
                           tags$img(src = "lowres-cb59057b0d4ef2a9ce75c19a128ca2ca.jpg",
                                    alt = "Noisy Miner photo",
                                    width = "100%"))),
           linknewtab(href = "https://birdlifephotography.org.au/index.php/show-image?return=search&single&id=19910",
                  class = "datalabels",
                  HTML("&copy;Con Boekel 2016 BirdLife Photography"))
  ),
  tags$p("Noisy Miners are aggressive native honeyeaters that are usually found in",
	tags$b("woodlands that lack midstorey vegetation"),
       "(shrubs and small trees 2-10m in height). In woodlands without midstorey, Noisy Miners are able to see and attack smaller birds, excluding them from the area.",
 "They are easy to recognise by their bright yellow eyes and beak, and their persistent, raucous call."),
    faqlink(faqid = "nmmidstorey", "Learn more")
)

)
),
	
   
  # WCF
twocolumns(heading = "Woody cover",
  left = tagList(
    tags$p("Bird occupancy depends heavily on the amount of woody vegetation cover",
   "(foliage cover greater than 2m high)",
    "within the woodland area and in the surrounding landscape."),
    tags$p("Estimate woody cover in and around your woodland area by identifying its location on the map."),
    infotext("Zoom to your farm and click to place a pin on the map, or enter latitude and longitude."),
    infotext("Select a representative year for your farm (between 1990 and 2019), then load woody vegetation cover amounts for this woodland area."),
  ),
  right = tagList(fluidRow(
      tags$h3("Select location"),
    column(4, 
      uiOutput(ns("latlonerror_short")),
      textInput(ns("lon"), "Longitude", value = attributes$usedlon, width = '100%',
                placeholder = ""),
      textInput(ns("lat"), "Latitude", value = attributes$usedlat, width = '100%',
                placeholder = ""),
      selectInput(ns("yearforcanopy"), "Representative year",
		  choices = c("", 2019:1990),
		  selected = if (isTruthy(attributes$usedyear)){attributes$usedyear}else{""}),
      tags$div(
	style = "display: -webkit-inline-flex;",
        actionButton_notdfl(ns("getwoodycanopy"), "Load woody cover", class = "btn-primary", style = "flex: 1;"),
        tags$span(id = ns("tickplace"),
            style = "width: 2rem; height: 2rem; vertical-align: middle;",
            style = "margin-top: auto; margin-bottom: auto;",
            style = "margin-left: 0.25rem; position: relative; display: inline;")
      )
    ),
    column(8, leaflet_UI(ns("leaflet"))),
  tags$div(class = "datalabels",
      "Woody cover estimates from the",
      linknewtab("ANU Centre for Water and Landscape Dynamics.", href = "http://wald.anu.edu.au/"),
      "See",
      linknewtab(href = 'http://anuwald.science/tree', "Tree Change portal"), 
      "and ", linknewtab(href = "https://doi.org/10.1016/j.jag.2020.102209",
                     "Liao et al. (IJAEOG, 2020).")),
  tags$div(style = "color:red; font-style:italic;", textOutput(ns("latlonerror"), inline = TRUE))
  ),
  tags$h3("Woody cover amounts"),
  # 500m WCF
  tags$div(
    tags$div(class = "bodysmall", "Nearby Woody Cover"),
    tags$div(class = "datalabels", "Percentage area of woody cover within 500m of the centre of the woodland area (including cover inside the woodland area)"),
    sliderInput(label = NULL,
                inputId = ns("pc_woody500m"),
                min = 2, max = 20, step = 0.5,
                width = "100%",
		post = "%",
                value = attributes$woody500m)
  ),
  #3000m WCF
  tags$div(
    tags$div(class = "bodysmall", "Regional Woody Cover"),
    tags$div(class = "datalabels", "Percentage area of woody cover within 3km of the woodland area"),
    sliderInput(label = NULL,
                inputId = ns("pc_woody3000m"),
                min = 2, max = 20, step = 0.5,
                width = "100%",
		post = "%",
                value = attributes$woody3000m)
  )
  )
))
  

acc_item <- accordion_item(title = paste("Woodland area", pid),
    id = ns("accitem"),
    internals,
    footer = tagList(
      actionButton_notdfl(ns("delete"), "Delete woodland area", icon = icon("trash"),
		   style = "color: #F4511E; background-color: #fcdcd2;"),
      do.call(actionButton_notdfl,
        args = c(list(ns("cancel"), "Cancel", class = "btn-outline-primary",
		 onclick = paste0("scrollupby('", ns("accitem"), "_body","')")),
             toggle_attr(paste0(ns("accitem"), "_body"))
             )),
      actionLink(ns("savewrap"),
      label = do.call(actionButton_notdfl,
              args = c(list(ns("save"), "Save and Close", 
		       onclick = paste0("scrollupby('", ns("accitem"), "_body","')"),
			    class = "btn-primary"),
		       if (patchcomplete){list()}else{list(class = "disabled", disabled = "")},
                       toggle_attr(paste0(ns("accitem"), "_body"))
              ))
      )
      ),
    footerdflt = "none",
    opentype = "edit"
    )
inattrisdefault <- patchequalsdefault(attributes)
if (inattrisdefault){acc_item <- expanditem(acc_item)}
return(acc_item)
}

patchattr_Server <- function(id, pid, selector, presentindicator, bbox){
  moduleServer(
    id,
    function(input, output, session){
      stopifnot(is.reactive(presentindicator))
      ns <- session$ns
      savedvals <- reactiveVal(NULL)
      showwarnings <- reactiveVal(FALSE)
      
      # keep presence of UI and savedvals up to date with presentindicator
      observeEvent(presentindicator(), {
        # all changes involve first deleting the UI and savedvals
        removeUI(paste0("#", ns("accitem"))) 
        savedvals(NULL)
        if (isTruthy(presentindicator())){
          if (is.numeric(presentindicator())){
              newUI <- patchattr_UI(ns, pid, defaultpatchvalues, patchcomplete = FALSE)
              insertUI(selector,
                       where = "beforeBegin",
                       ui = newUI
              )
              savedvals(c(defaultpatchvalues, list(pid = pid)))
          }
          if (is.list(presentindicator())){
            newattr <- presentindicator()
	    patchcomplete <- isTruthy(newattr$nm) & isTruthy(newattr$woodlandtype)
            newUI <- patchattr_UI(ns, pid, newattr, patchcomplete = patchcomplete)
            insertUI(selector,
                     where = "beforeBegin",
                     ui = newUI
            )
            newattr$pid = pid
            savedvals(as.list(newattr)) #as.list converts data frame to list of columns
          }
        }
      }, ignoreInit = FALSE, ignoreNULL = FALSE) #so at the very start of the app a woodland area can be added
      
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
        }) %>% debounce(2000) # to stop heaps of clicking doing things
      observeEvent(getwoodycanopy_d(), {
        session$sendCustomMessage("getwoodycanopyfromlatlon", "nothing") # for google tracking
        latlonerror("")
        latlonerror_short("")
        wcfs <- tryCatch( #finish with NULL WCF if there is an error - that will make tick turn to ban
          {
            lat <- parsechar(input$lat, "Latitude")
            lon <- parsechar(input$lon, "Longitude")
            year <- parsechar(input$yearforcanopy, "Year")
            wcfs <- canopyfromlatlon(lon,lat,year)
            checkfinalwcfs(wcfs)
            wcfs
          },
          error = function(e) {
      	    if (grepl("(^Lat|Lon)", e$message)){
              latlonerror_short("Please select a location")
              latlonerror(e$message)
      	    } else if (grepl("Year", e$message)){
              latlonerror_short("Please choose a year")
      	      latlonerror(e$message)
      	    } else {
      	      latlonerror(paste("Error: ", e$message))
      	    }
            pc_woody500m_latlon(NULL)
            pc_woody3000m_latlon(NULL)
            return(NULL)
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
        } else {
              insertUI(paste0("#", ns("tickplace")),
                where = "afterBegin",
                ui = tags$span(id = ns("tick"), 
                              icon("ban", style = "font-size: 2rem; color: #FF0000; position: absolute;"),
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
        presentindicator(NULL)
        savedvals(NULL)
      }, ignoreInit = TRUE)
      
      # checks
      observe({shinyjs::toggleState(id = "save", 
                 condition = (isTruthy(input$nm) & isTruthy(input$woodlandtype)))})
      observeEvent(input$savewrap, { #error messages even when 'save' is disabled
	if (isTruthy(input$nm) & (!isTruthy(input$woodlandtype))){
	  showNotification("Please specify woodland type", type = "error")
	} else if ((!isTruthy(input$nm)) & isTruthy(input$woodlandtype)){
	  showNotification("Please specify Noisy Miner presence", type = "error")
        } else if ((!isTruthy(input$nm)) & (!isTruthy(input$woodlandtype))){
	  showNotification("Please specify both woodland type and Noisy Miner presence", type = "error")
        }
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
  observeEvent(c(input$save, input$delete), {
    session$doBookmark()
  }, priority = -100)  
      
      # convert out info
      reactive({
        out <- savedvals()
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

  shinyApp(    {fluidPage(
    tags$head(tags$style(appcss),
	      tags$link(href="https://fonts.googleapis.com/css?family=Poppins|Inter", rel="stylesheet"),
              includeHTML("./www/extra.html"), #has the toggleexpand function
	      ),
    tags$div(id = "placeholder"))
  },
           function(input, output, session){
             presentindicator <- reactiveVal(0)
             bbox <- reactive({bbox_allregions})
             refresh <- reactiveTimer(1000 * 7)
             refresh2 <- reactiveTimer(1000 * 10)
             observeEvent(refresh(),{
              presentindicator(runif(1))
              showNotification("present altered")
              })
             observeEvent(refresh2(),{
               presentindicator(defaultpatchvalues * 2)
               showNotification("present altered")
             })
             patchattr_Server("patchattr",
                              pid = 1,
                              selector = paste("#", "placeholder"),
                              presentindicator = presentindicator,
                              bbox = bbox)}
  )
}
