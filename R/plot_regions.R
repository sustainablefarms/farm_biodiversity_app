state_borders <- readRDS("./data/state_borders.rds")
regionpts <- readRDS("data/sa2_points_climate.rds")
regionpolygons <- readRDS("data/sa2_polygons.rds") %>%
  dplyr::filter(SA2_NAME16 %in% regionpts$label)
# ns = function(x)x
# regionplot_borders(ns)

regionplot_dots <- function(source = "region_map"){
  out <- plotly::plot_ly(
    regionpts,
    x = ~longitude,
    y = ~latitude,
    type = "scatter",
    mode = "markers",
    source = source,
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


regionplot_borders <- function(source = "region_map"){
  out <- plot_ly(source = source) %>% 
    add_sf(data = state_borders,
           type = "scatter",
           mode = "lines",
           inherit = FALSE,
           showlegend = FALSE,
           hoverinfo = 'none',
           colors = RColorBrewer::brewer.pal(4, "Blues"), #needed this to stop warning with auto color
           line = list(color = "gray")
    ) %>%
    add_sf(data = regionpolygons,
          type = "scatter",
          mode = "lines",
          inherit = FALSE,
          color = ~SA2_NAME16, #so that the SA2_NAME16 is registered as a data value
          colors = RColorBrewer::brewer.pal(4, "Blues"), #needed this to stop warning with auto color
          alpha = I(0),
          stroke = I("black"),
          text = ~SA2_NAME16,
          hoverinfo = 'text',
          hoveron='points+fills',
          key = ~SA2_NAME16,
          hovertemplate = paste('%{text}<extra></extra>'),
          # strokes = RColorBrewer::brewer.pal(4, "Blues"),
          showlegend = FALSE
         ) %>%

    add_text(x = min(regionpts$longitude)+1, 
             y = c(max(regionpts$latitude)-1, min(regionpts$latitude) - 0.5),
             text = c("NSW", "VIC"), 
             textfont = list(size = 20),
             showlegend = FALSE,
             inherit = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = FALSE),
      yaxis = list(#scaleanchor = "x",
                   title = "", showline = FALSE, showticklabels = FALSE, showgrid = FALSE, fixedrange = FALSE),
      margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
      # clickmode = "event+select", #so polygons will be both event clicked and 'selected'
      dragmode = "pan",
      paper_bgcolor='transparent',
      plot_bgcolor = 'transparent'
    ) %>%
    plotly::config(displayModeBar = FALSE,
                   scrollZoom = TRUE,
                   doubleClick = FALSE) %>%
    plotly::event_register(event = 'plotly_click')
  out %>% #the below prints info to the 'inspect element, console panel
    htmlwidgets::onRender("
    function(el) {
      el.on('plotly_hover', function(d) {
        console.log('Hover: ', d);
      });
      el.on('plotly_click', function(d) {
        console.log('Click: ', d);
      });
      el.on('plotly_selected', function(d) {
        console.log('Select: ', d);
      });
    }
  ")
  return(out)
}