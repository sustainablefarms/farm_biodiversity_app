species_plotly <- function(df, title = "", add_plus = FALSE, errorbar = FALSE){
  set.seed(1)
  df <- df[order(df$value), ]
  # df$species <- factor(
  #   seq_len(10),
  #   levels = seq_len(10),
  #   labels = df$species)
  if(add_plus){
    df$label <- paste0("+", round(df$value * 100, 0), "%")
  }else{
    df$label <- paste0("", round(df$value * 100, 0), "%")
  }
  
  df$tooltip <- paste0(df$species, " has some interest features.")
  if (errorbar){
    plt <- plot_ly(data = df,
            y = ~species,
            x = ~value,
            color = ~value,
            marker = list(colorscale = "Blues", #"[[0, '#006666ff'], [1, '#178BCAff']]", 
                          reversescale = TRUE, cmax = 1.2, cmin = 0.3), #cmin = 0, cmax = 1),
            # text = paste0(round(data$value, digits = 2) * 100, "%"),
            text = df$tooltip,
            # hoverinfo = 'text',
            # textposition = 'auto',
            hovertemplate = paste('<b>%{text}</b><extra></extra>'), #the <extra></extra> removes the 'trace 0' extra information
            error_x = ~list(type = 'data',
                            array = upper - value,
                            arrayminus = value - lower,
                            symmetric = FALSE,
                            color = '#000000'),
            type = "bar") %>%
      add_annotations(x  = ~lower, 
                      y = ~species, 
                      text = df$label,
                      xanchor = "right",
                      xshift = -3,
                      bgcolor = "rgba(255,255,255,1)",
                      showarrow = FALSE)
  } else {
    plt <- plot_ly(data = df,
                   y = ~species,
                   x = ~value,
                   color = ~value,
                   marker = list(colorscale = "Blues", #"[[0, '#006666ff'], [1, '#178BCAff']]", 
                                 reversescale = TRUE, cmax = 1.2, cmin = 0.3), #cmin = 0, cmax = 1),
                   # text = paste0(round(data$value, digits = 2) * 100, "%"),
                   text = df$tooltip,
                   # hoverinfo = 'text',
                   # textposition = 'auto',
                   hovertemplate = paste('<b>%{text}</b><extra></extra>'), #the <extra></extra> removes the 'trace 0' extra information
                   type = "bar") %>%
           add_annotations(x  = ~value, 
                    y = ~species, 
                    text = df$label,
                    xanchor = "right",
                    xshift = -3,
                    bgcolor = "rgba(255,255,255,1)",
                    showarrow = FALSE)
  }
  plt <- plt %>%
    add_annotations(x  = 0, 
                    y = ~species, 
                    text = ~species,
                    xanchor = "left",
                    xshift = 3,
                    font = list(color = "rgba(255,255,255,1)"),
                    showarrow = FALSE) %>%
    layout(yaxis = ~list(categoryorder = "array", categoryarray = value, visible = FALSE)) %>%
    layout(xaxis = list(visible = FALSE),
           margin = list(l = 0, r = 0, t = 0, b = 0)) %>%
    hide_colorbar() %>%
    config(displayModeBar = FALSE)
  return(plt)
}