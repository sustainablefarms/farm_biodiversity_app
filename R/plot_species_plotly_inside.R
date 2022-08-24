# create plotly plot of bars of the 'value' column. Coloured by the 'value' column too.
add_trace_colorbarsbyvalue <- function(p, data, pal = defaultpal(data$value)){
  if (!("pattern_shape" %in% names(data))){data$pattern_shape <- ""}
  plt <- p %>% #initiate plot
    add_trace(data = data,
              type = "bar",  #make a bar plot
              y = ~species,
              x = ~value,
              marker = list(color = ~pal(value),
                            pattern = list(shape = ~pattern_shape,
                                           fillmode = "overlay"),
                            line = list(color = ~pal(value))),
              showlegend = FALSE,
              text = ~species,
              textposition = "none", #turns off text display
              hovertext = ~label, #from provided data
              hoverinfo = "none" #but turn hovering off by default
    )
  return(plt)
}

defaultpal <- function(value){
  pal <- scales::col_numeric(c(appcolors[["Green 10"]], appcolors[["Dark Green"]]),
                             domain = value)
  return(pal)
}

plot_ly_yinside <- function(df){
  textcolcut <- mean(range(df$value))
  palopp <- function(values){
    cols <- rep("#FFFFFF", length(values))
    cols[values < textcolcut] <- appcolors[["Dark Green"]]
    return(cols)
  }
  plt <- plot_ly() %>%
    add_trace_colorbarsbyvalue(df) %>%
  style(
              textposition = "inside",
              insidetextanchor = "start",
              insidetextfont = list(color = ~palopp(value))
  )

  plt %>%
    plotly::layout(
      yaxis = list(visible = FALSE, type = "category")
    )
}
