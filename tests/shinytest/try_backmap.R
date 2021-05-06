data <- list()
data$points <- readRDS("data/sa2_points_climate.rds")

library(sf)
latlons <- data$points %>%
  sf::st_as_sf(coords = c("longitude", "latitude"),
               remove = FALSE,
               crs = "+proj=longlat +datum=WGS84 +no_defs")
ranges <- apply(latlons[, c("longitude", "latitude")], MARGIN = 2, range)

# plot points on map of Australia
ausstates <- readRDS("./data/state_borders.rds")


plotly::plot_ly(
  data$points,
  x = ~longitude,
  y = ~latitude,
  type = "scatter",
  mode = "markers",
  source = "region_map",
  marker = list(
    size = 10,
    color = ~color
  ),
  hoverinfo = "text",
  text = ~label
) %>% plotly::layout(
  xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
  yaxis = list(scaleanchor = "x",
               title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE),
  margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
  paper_bgcolor='transparent',
  plot_bgcolor = 'transparent'
) %>%
  plotly::config(displayModeBar = FALSE) %>%
  add_sf(data = ausstates, type = "scatter", mode = "lines", inherit = FALSE, line = list(color = "gray")) %>%
  add_text(x = c(min(data$points$longitude), 148.5), 
           y = c(max(data$points$latitude)-1, min(data$points$latitude) - 0.5),
           text = c("NSW", "VIC"), 
           textfont = list(size = 20),
           inherit = FALSE)
