# leaflet module. Good help here: https://rstudio.github.io/leaflet/shiny.html

library(shiny)
library(leaflet)

ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output, session) {
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
      addProviderTiles("Esri.WorldImagery", group = "Imagery") %>%
      addLayersControl(
        baseGroups = c("Map", "Imagery")) %>%
      fitBounds(bbox[["xmin"]], bbox[["ymin"]],
                  bbox[["xmax"]], bbox[["ymax"]])
  })
  
  observe({
    showNotification(paste(input$mymap_click, collapse = " "))
  })
}

shinyApp(ui, server)
