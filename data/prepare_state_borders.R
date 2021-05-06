# prepare cut down state map
# ausstates <- readRDS("../sflddata/private/data/basemaps/ausstates.rds") %>%
#   sf::st_transform(st_crs(latlons)) %>%
#   sf::st_cast("MULTILINESTRING") %>%
#   sf::st_crop(ymin = -39.5, ymax = -32, xmin = 144, xmax = 152) %>%
#   st_simplify(dTolerance = 0.05) %>%
#   sf::st_cast("MULTILINESTRING") 
# saveRDS(ausstates, file = "./data/state_borders.rds")