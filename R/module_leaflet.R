# leaflet module. Good help here: https://rstudio.github.io/leaflet/shiny.html
# it appears that sufficient attribution is included as part of the map automatically
# see:  https://github.com/leaflet-extras/leaflet-providers

leaflet_UI <- function(id){ 
  ns <- NS(id)
  fluidPage(leaflet::leafletOutput(ns("mymap")))
}

leaflet_Server <- function(id, refresh, selected_region){
  moduleServer(
    id,
    function(input, output, session){
  region_polygons <- readRDS("data/sa2_polygons.rds") 
  # polygons[region_polygons$SA2_NAME16 == outOfModule()$selected_region, ]
  region_polygons <- region_polygons %>% sf::st_transform(4326)
  bbox_allregions <- sf::st_bbox(region_polygons)
  bbox <- reactiveValues(
    xmin = bbox_allregions[["xmin"]],
    xmax = bbox_allregions[["xmax"]],
    ymin = bbox_allregions[["ymin"]],
    ymax = bbox_allregions[["ymax"]]
  )
  updatebbox <- reactive({
    if (isTruthy(selected_region())){
      roi <- region_polygons[region_polygons$SA2_NAME16 == selected_region(), ]
      bboxtmp <- sf::st_bbox(roi)
      bbox$xmin <- bboxtmp[["xmin"]]
      bbox$xmax <- bboxtmp[["xmax"]]
      bbox$ymin <- bboxtmp[["ymin"]]
      bbox$ymax <- bboxtmp[["ymax"]]
    }
  })
  
  output$mymap <- renderLeaflet({
    updatebbox()
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
    leaflet::leaflet() %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery Powered by Esri") %>%
      leaflet::addTiles(group = "Map") %>%
      leaflet::addLayersControl(
        baseGroups = c("Map", "Imagery powered by Esri")) %>%
      leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                  bbox[["xmax"]], bbox[["ymax"]])
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
