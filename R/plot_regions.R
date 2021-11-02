state_borders <- readRDS("./data/state_borders.rds")
regionpts <- readRDS("data/sa2_points_climate.rds")
regionpolygons_4326 <- readRDS("data/sa2_polygons.rds") %>%
  dplyr::filter(SA2_NAME16 %in% regionpts$label) %>%
  sf::st_transform(4326) # %>%
  # sf::st_simplify(preserveTopology = TRUE,
  #                 dTolerance = 1000)

regionplot_leaflet <- function(){
  leaflet::leaflet(regionpolygons_4326) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      # popup = ~SA2_NAME16,
      label = ~SA2_NAME16,
      highlightOptions = leaflet::highlightOptions(color = "red")
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