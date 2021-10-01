# leaflet module. Good help here: https://rstudio.github.io/leaflet/shiny.html
# it appears that sufficient attribution is included as part of the map automatically
# see:  https://github.com/leaflet-extras/leaflet-providers

library(shiny)
library(leaflet)

leaflet_UI <- function(id){ 
  ns <- NS(id)
  fluidPage(leafletOutput(ns("mymap")))
}

leaflet_Server <- function(id, refresh){
  moduleServer(
    id,
    function(input, output, session){
  region_polygons <- readRDS("data/sa2_polygons.rds") 
  # polygons[region_polygons$SA2_NAME16 == outOfModule()$selected_region, ]
  roi <- region_polygons %>% sf::st_transform(4326)
  bbox <- sf::st_bbox(roi)
  
  output$mymap <- renderLeaflet({
    # sensitivity to something that makes the map be regenerated
    if (is.reactivevalues(refresh)){
      vals <- reactiveValuesToList(refresh) 
    } else if (is.reactive(refresh)) {
      vals <- refresh()
    } else {
      vals <- refresh
    }
    # for some reason refreshing is needed for map clicks to be noticed.
    
    
    # showNotification("Leaflet generated")
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery Powered by Esri") %>%
      addTiles(group = "Map") %>%
      addLayersControl(
        baseGroups = c("Map", "Imagery powered by Esri")) %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                  bbox[["xmax"]], bbox[["ymax"]])
  })
  
  observe({
    validate(need(input$mymap_click, ""))
    # showNotification(paste(input$mymap_click, collapse = " "))
    leafletProxy("mymap") %>%
      removeMarker("newpatch_marker") %>%
      addMarkers(lng = input$mymap_click$lng,
                      lat = input$mymap_click$lat,
                      layerId = "newpatch_marker")
  })
  reactive(input$mymap_click)
})}

app_leaflet <- function(){
  shinyApp(leaflet_UI("leaflet"),
  function(input, output, session){leaflet_Server("leaflet", "NA")}
  )
}
