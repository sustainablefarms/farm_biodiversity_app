state_borders <- readRDS("./data/state_borders.rds")
regionpts <- readRDS("data/sa2_points_climate.rds")
regionpolygons_4326 <- readRDS("data/sa2_polygons.rds") %>%
  dplyr::filter(SA2_NAME16 %in% regionpts$label) %>%
  sf::st_transform(4326) # %>%
  # sf::st_simplify(preserveTopology = TRUE,
  #                 dTolerance = 1000)

# for Leaflet spinner, from https://community.rstudio.com/t/cant-get-leaflet-spin-plugin-working-in-r-shiny/64556
# https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
# https://stackoverflow.com/questions/52846472/leaflet-plugin-and-leafletproxy-with-polylinedecorator-as-example
spinPlugin <- htmltools::htmlDependency(
  "spin.js",
  "2.3.2",
  src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/spin.js/2.3.2"),
  script = "spin.min.js") # there's no spin.css

leafletspinPlugin <- htmltools::htmlDependency(
  "Leaflet.Spin",
  "1.1.2",
  src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/Leaflet.Spin/1.1.2"),
  script = "leaflet.spin.min.js")

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

# Note: Ctrl-Shift-J opens the javascript console in the browser

regionplot_leaflet_init <- function(){
  leaflet::leaflet(regionpolygons_4326,
                   options = leaflet::leafletOptions(
                     zoomControl = FALSE
                   )) %>%
    leaflet::addTiles() %>%
    registerPlugin(spinPlugin) %>%
    registerPlugin(leafletspinPlugin) %>%
    htmlwidgets::onRender("spin_event") %>% #calls the function
    leaflet::clearShapes() %>%
    leaflet::addPolygons(
      # popup = ~SA2_NAME16,
      label = ~SA2_NAME16,
      highlightOptions = leaflet::highlightOptions(color = "red")
    )
}

regionplot_leaflet_addpolys <- function(obj){
  obj %>%
    leaflet::clearShapes() %>%
    leaflet::addPolygons(
      # popup = ~SA2_NAME16,
      label = ~SA2_NAME16,
      highlightOptions = leaflet::highlightOptions(color = "red"),
      data = regionpolygons_4326
    )
}

lonlat2region <- function(lon, lat){
  pt <- sf::st_point(x = c(lon, lat), dim = "XY")
  pt_sfc <- sf::st_sfc(pt, crs = 4326)
  containingregions <- sf::st_contains(regionpolygons_4326, pt_sfc, sparse = FALSE)
  idx <- which(containingregions) #integer(0) when no matches
  selected_region <- regionpolygons_4326$SA2_NAME16[idx]
  return(selected_region)
}
