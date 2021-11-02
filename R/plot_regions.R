state_borders <- readRDS("./data/state_borders.rds")
regionpts <- readRDS("data/sa2_points_climate.rds")
regionpolygons <- readRDS("data/sa2_polygons.rds") %>%
  dplyr::filter(SA2_NAME16 %in% regionpts$label)
# regionplot_dots(function(x)x)
ns = function(x)x

regionplot_dots <- function(ns){
  out <- plotly::plot_ly(
    regionpts,
    x = ~longitude,
    y = ~latitude,
    type = "scatter",
    mode = "markers",
    source = ns("region_map"),
    marker = list(
      size = 10,
      color = ~color
    ),
    hoverinfo = "text",
    text = ~label
  ) %>%
    add_sf(data = state_borders, 
           type = "scatter", 
           mode = "lines",
           inherit = FALSE,
           showlegend = FALSE,
           hoverinfo = 'none',
           line = list(color = "gray")
  ) %>% 
add_text(x = min(regionpts$longitude)+1, 
   y = c(max(regionpts$latitude)-1, min(regionpts$latitude) - 0.5),
   text = c("NSW", "VIC"), 
   textfont = list(size = 20),
   showlegend = FALSE,
   inherit = FALSE
   ) %>%
    plotly::layout(
    xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
    yaxis = list(scaleanchor = "x",
                 title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
dragmode = FALSE,
    paper_bgcolor='transparent',
    plot_bgcolor = 'transparent'
  ) %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::event_register(event = 'plotly_click')
  return(out)
}


regionplot_borders <- function(ns){
  plot_ly() %>%
    add_sf(data = regionpolygons,
          type = "scatter",
          mode = "lines",
          color = ~SA2_NAME16,
          fill = "toself",
          hoveron = "fills"
          # hoverinfo = 'text',
          # text = ~SA2_NAME16
          )
    style(hoverinfo = TRUE,
          hovertext = regionpolygons[ , "SA2_NAME16", drop = TRUE])
  
    add_sf(data = ,
           hoverinfo = )
          type = )
  out <- plotly::plot_ly(
    regionpts,
    x = ~longitude,
    y = ~latitude,
    type = "scatter",
    mode = "markers",
    source = ns("region_map"),
    marker = list(
      size = 10,
      color = ~color
    ),
    hoverinfo = "text",
    text = ~label
  ) %>%
    add_sf(data = state_borders, 
           type = "scatter", 
           mode = "lines",
           inherit = FALSE,
           showlegend = FALSE,
           hoverinfo = 'none',
           line = list(color = "gray")
    ) %>% 
    add_text(x = min(regionpts$longitude)+1, 
             y = c(max(regionpts$latitude)-1, min(regionpts$latitude) - 0.5),
             text = c("NSW", "VIC"), 
             textfont = list(size = 20),
             showlegend = FALSE,
             inherit = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
      yaxis = list(scaleanchor = "x",
                   title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = TRUE),
      margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
      dragmode = FALSE,
      paper_bgcolor='transparent',
      plot_bgcolor = 'transparent'
    ) %>%
    plotly::config(displayModeBar = FALSE) %>%
    plotly::event_register(event = 'plotly_click')
  return(out)
}