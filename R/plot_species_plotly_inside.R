# create plotly plot of bars of the 'value' column. Coloured by the 'value' column too.
plot_ly_colorbarsbyvalue <- function(df){
  df$pattern_shape <- dplyr::case_when(
    df$value >= 0 ~ "",
    TRUE ~ "x")
  pal <- scales::col_numeric(c(appcolors[["Green 10"]], appcolors[["Dark Green"]]),
                             domain = df$value)
  plt <- plot_ly(data = df) %>% #initiate plot
    add_trace(type = "bar",  #make a bar plot
              y = ~species,
              x = ~value,
              marker = list(color = ~pal(value),
                            pattern = list(shape = ~pattern_shape,
                                           fillmode = "overlay")),
              showlegend = FALSE,
              text = ~species,
              textposition = "none", #turns off text display
              hovertext = ~label, #from provided data
              hoverinfo = "none" #but turn hovering off by default
    )
  return(plt)
}

plot_ly_yinside <- function(df){
  textcolcut <- mean(range(df$value))
  palopp <- function(values){
    cols <- rep("#FFFFFF", length(values))
    cols[values < textcolcut] <- appcolors[["Dark Green"]]
    return(cols)
  }
  plt <- plot_ly_colorbarsbyvalue(df) %>%
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
