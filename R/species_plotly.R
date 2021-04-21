plot_ly_specroot <- function(df){
  pal <- scales::col_numeric(c("#d0e7f4", "#178BCA"),
                      domain = df$value)
  palopp <- function(values){
    cols <- pal(values)
    rgbs <- col2rgb(cols)/255
    Luvs <- convertColor(t(rgbs), from = "sRGB", to = "Luv")
    lvls <- Luvs[, "L"]
    lvls[Luvs[, "L"] < 70] <- 100 - lvls[Luvs[, "L"] < 70] / 8
    lvls[Luvs[, "L"] >= 70] <- (100 - lvls[Luvs[, "L"] >= 70]) / 4
    greys <- grey(lvls/100)
    return(greys)
  }
  plt <- plot_ly(data = df) %>% #initiate plot
    add_trace(type = "bar",  #make a bar plot
              y = ~species,
              x = ~value,
              marker = list(color = ~pal(value)),
              showlegend = FALSE,
              text = ~species,
              textposition = "inside",
              insidetextanchor = "start",
              insidetextfont = list(color = ~palopp(value))
    ) %>%
    # tooltips
    style(hoverinfo = TRUE,
          hovertext = df$tooltip,
          hoverlabel = list(bgcolor = "white",
                            font = list(color = "black",
                                        size = 12)),
          hovertemplate = paste('%{hovertext}<extra></extra>')) %>% #the <extra></extra> removes the 'trace 0' extra information
  # alter layout
  plotly::layout(xaxis = list(visible = FALSE),
         yaxis = list(visible = FALSE),
         margin = list(l = 0, r = 0, t = 0, b = 0)) %>%
  hide_colorbar()
  # add the species names
    # add_annotations(x  = 0,
    #                 y = ~species,
    #                 text = ~species,
    #                 xanchor = "left",
    #                 xshift = 3,
    #                 bgcolor = ~palopp(value),
    #                 textfont = list(color = ~palopp(value)),
    #                 showarrow = FALSE) %>%
  return(plt)
}

species_plotly_common <- function(df){
  set.seed(1)
  df <- topnrows(df, 10, "value")
  df$label <- paste0("", round(df$value * 100, 0), "%")
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  plot_ly_specroot(df) %>%
    # add error bars
    style(error_x = list(visible = TRUE,
                         type = 'data',
                         array = df$upper - df$value,
                         arrayminus = df$value - df$lower,
                         symmetric = FALSE,
                         color = '#000000')) %>%
    # add the values onto the bars
    add_annotations(x  = ~lower, 
                    y = ~species, 
                    text = df$label,
                    xanchor = "right",
                    xshift = -3,
                    font = list(color = "rgba(0,0,0,1)"),
                    bgcolor = "rgba(255,255,255,1)",
                    showarrow = FALSE,
                    showlegend = FALSE) %>%
  # alter order
  plotly::layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed"))
  #despite the descriptions, the above line actually seems to order things as they are given in df
}

species_plotly_different <- function(df){
  set.seed(1)
  df <- topnrows(df, 10, "value")
  df$label <- paste0("x ", round(df$value, 2))
  df$tooltip <- paste0(df$species, " has some interest features.")
  plot_ly_specroot(df) %>%
  # add the values onto the bars
  add_annotations(x  = ~value, 
                  y = ~species, 
                  text = df$label,
                  xanchor = "right",
                  xshift = -3,
                  bgcolor = "rgba(255,255,255,1)",
                  showarrow = FALSE,
                  showlegend = FALSE) %>%
  # alter order
  plotly::layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed")) 
}

species_plotly_both <- function(species_prob_current, spec_different){
  cmn <- species_plotly_common(tocommon(species_prob_current))
  dft <- species_plotly_different(spec_different)
  subplot(cmn, dft) %>%
    plotly::config(displayModeBar = FALSE)
}

species_plotly_modal <- function(species_prob_current, spec_different){
  species_prob_current <- data.frame(species = rownames(species_prob_current), species_prob_current)
  
  df_both <- dplyr::full_join(species_prob_current, spec_different, by = "species", suffix = c(".cur", ".ref"))
  
  traits <- get("traits", envir = globalenv())
  df_both <- dplyr::left_join(df_both, traits, by = c(species = "Common Name"))
  
  # arrange input data frames
  df_both <- df_both %>% dplyr::arrange(- `Body Length`)
  
  df_probs <- df_both[, c("species", "lower", "upper", "value.cur", "bestsite", "Body Length", "Body Mass")]
  colnames(df_probs)[4] <- "value"
  

  probsplt <- plot_ly_specroot(df_probs) %>%
    # add error bars
    style(error_x = list(visible = TRUE,
                         type = 'data',
                         array = df_probs$upper - df_probs$value,
                         arrayminus = df_probs$value - df_probs$lower,
                         symmetric = FALSE,
                         color = '#000000')) %>%
    layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = TRUE))
  
  df_ratio <- df_both[, c("species", "lower", "upper", "value", "bestsite", "Body Length", "Body Mass")]
  colnames(df_ratio)[4] <- "value"
  ratioplt <- plot_ly_specroot(df_ratio) %>%
    layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = TRUE))
  
  subplot(probsplt, ratioplt, shareY = TRUE) %>%
    plotly::config(displayModeBar = FALSE)
}
