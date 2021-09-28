# leaflet module. Good help here: https://rstudio.github.io/leaflet/shiny.html
# it appears that sufficient attribution is included as part of the map automatically
# see:  https://github.com/leaflet-extras/leaflet-providers

library(shiny)
library(leaflet)

leaflet_UI <- function(id){ 
  ns <- NS(id)
  fluidPage(leafletOutput(ns("mymap")))
}

leaflet_Server <- function(id){
  moduleServer(
    id,
    function(input, output, session){
  region_polygons <- readRDS("data/sa2_polygons.rds") 
  # polygons[region_polygons$SA2_NAME16 == outOfModule()$selected_region, ]
  roi <- region_polygons %>% sf::st_transform(4326)
  bbox <- sf::st_bbox(roi)
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery Powered by Esri") %>%
      addLayersControl(
        baseGroups = c("Map", "Imagery powered by Esri")) %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                  bbox[["xmax"]], bbox[["ymax"]])
  })
  
  observe({
    validate(need(input$mymap_click, ""))
    leafletProxy("mymap") %>%
      removeMarker("newpatch_marker") %>%
      addCircleMarkers(lng = input$mymap_click$lng,
                      lat = input$mymap_click$lat,
                      layerId = "newpatch_marker")
  })
  reactive(input$mymap_click)
})}

app_leaflet <- function(){
  shinyApp(leaflet_UI("leaflet"),
  function(input, output, session){leaflet_Server("leaflet")}
  )
}
