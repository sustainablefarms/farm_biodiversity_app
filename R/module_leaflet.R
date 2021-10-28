# leaflet module. Good help here: https://rstudio.github.io/leaflet/shiny.html
# it appears that sufficient attribution is included as part of the map automatically
# see:  https://github.com/leaflet-extras/leaflet-providers

leaflet_UI <- function(id){ 
  ns <- NS(id)
  fluidPage(leaflet::leafletOutput(ns("mymap")))
}

leaflet_Server <- function(id, bbox){
  moduleServer(
    id,
    function(input, output, session){

  
  output$mymap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery Powered by Esri") %>%
      leaflet::addTiles(group = "Map") %>%
      leaflet::addLayersControl( 
        baseGroups = c("Map", "Imagery powered by Esri"))   %>%
      leaflet::fitBounds(bbox_allregions[["xmin"]], bbox_allregions[["ymin"]],
                         bbox_allregions[["xmax"]], bbox_allregions[["ymax"]])
    })
  
  observe({
    leaflet::leafletProxy("mymap") %>%
      leaflet::fitBounds(bbox()[["xmin"]], bbox()[["ymin"]],
                         bbox()[["xmax"]], bbox()[["ymax"]])
  })
  
  observe({
    validate(need(input$mymap_click, ""))
    # showNotification(paste(input$mymap_click, collapse = " "))
    leaflet::leafletProxy("mymap") %>%
      leaflet::removeMarker("newpatch_marker") %>%
      leaflet::addMarkers(lng = input$mymap_click$lng,
                      lat = input$mymap_click$lat,
                      layerId = "newpatch_marker")
  })
  
  setBookmarkExclude(c("mymap_click", 
                       "mymap_bounds",
                       "mymap_zoom",
                       "mymap_center",
                       "mymap_groups"
                       ))
  
  reactive(input$mymap_click)
})}

app_leaflet <- function(){
  shinyApp(leaflet_UI("leaflet"),
  function(input, output, session){leaflet_Server("leaflet", "NA")}
  )
}
