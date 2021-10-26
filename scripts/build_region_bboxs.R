# make bbox info for polygons
region_polygons <- readRDS("data/sa2_polygons.rds") 
# polygons[region_polygons$SA2_NAME16 == outOfModule()$selected_region, ]
region_polygons <- region_polygons %>% sf::st_transform(4326)
bbox_allregions <- sf::st_bbox(region_polygons)
bbox_regions <- lapply(region_polygons$geometry, sf::st_bbox)
names(bbox_regions) <- region_polygons$SA2_NAME16
saveRDS(bbox_regions, "./data/sa2_polygons_bbox.rds")
saveRDS(bbox_allregions, "./data/sa2_polygons_bboxtotal.rds")