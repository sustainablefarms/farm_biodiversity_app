plot_ly_yinside <- function(df){
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
    )
  plt %>%
    plotly::layout(
      yaxis = list(visible = FALSE, type = "category")
    )
}

plot_ly_youtside <- function(df, log2 = FALSE){
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  if (log2){
    df$label <- paste0(round((df$value - 1) * 100, 0), "%")
    df$value <- log2(df$value)
    df$pattern_shape <- dplyr::case_when(
      df$value >= 0 ~ "",
      TRUE ~ "x")
  } else {
    df$label <- paste0("", round(df$value * 100, 0), "%")
    df$pattern_shape = ""
  }
  
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
              marker = list(color = ~pal(value),
                            pattern = list(shape = ~pattern_shape)),
              showlegend = FALSE
    )
  plt %>%
    plotly::layout(
      yaxis = list(title = "", visible = TRUE, type = "category")
    )
}

fixed_layout <- function(p){
  p %>%
  plotly::layout(xaxis = list(visible = FALSE, fixedrange = TRUE),
                 yaxis = list(fixedrange = TRUE),
                 dragmode = FALSE,
                 margin = list(l = 0, r = 0, t = 0, b = 0)) %>%
    hide_colorbar()  %>%
    plotly::config(displayModeBar = FALSE)
}

add_tooltips <- function(p){
  df <- plotly_data(p)
  p %>%
    style(hoverinfo = TRUE,
        hovertext = df$tooltip,
        hoverlabel = list(bgcolor = "white",
                          font = list(color = "black",
                                      size = 12)),
        hovertemplate = paste('%{hovertext}<extra></extra>'))
}

add_error <- function(p){
  df <- plotly_data(p)
  p %>% 
    style(error_x = list(visible = TRUE,
                         type = 'data',
                         array = df$upper - df$value,
                         arrayminus = df$value - df$lower,
                         symmetric = FALSE,
                         color = '#000000'))
}

add_label_onleft <- function(p){
  errorbarsshown <- isTRUE(p$x$data[[1]]$error_x$visible)
  x <- p$x$data[[1]]$x
  if (errorbarsshown){
    x <- x - p$x$data[[1]]$error_x$arrayminus
  }
  p %>%
    add_annotations(x  = x, 
                  y = ~species, 
                  text = ~label,
                  xanchor = "right",
                  xshift = -3,
                  font = list(color = "rgba(0,0,0,1)"),
                  bgcolor = "rgba(255,255,255,1)",
                  showarrow = FALSE,
                  showlegend = FALSE) 
}

add_label_onright <- function(p){
  errorbarsshown <- isTRUE(p$x$data[[1]]$error_x$visible)
  x <- p$x$data[[1]]$x
  if (errorbarsshown){
    x <- x + p$x$data[[1]]$error_x$array
  }
  p %>%
    add_annotations(x  = x, 
                    y = ~species, 
                    text = ~label,
                    xanchor = "left",
                    xshift = 3,
                    font = list(color = "rgba(0,0,0,1)"),
                    bgcolor = "rgba(255,255,255,1)",
                    showarrow = FALSE,
                    showlegend = FALSE) 
}

add_label_onsignside <- function(p){
  errorbarsshown <- isTRUE(p$x$data[[1]]$error_x$visible)
  x <- p$x$data[[1]]$x
  if (errorbarsshown){
    x <- dplyr::case_when(
      x >= 0 ~ x + p$x$data[[1]]$error_x$array,
      x - p$x$data[[1]]$error_x$arrayminus)
  }
  xanchor <- dplyr::case_when(
      x > 0 ~ "left",
      TRUE ~ "right"
    )
  p %>%
    add_annotations(x  = x, 
                    y = ~species, 
                    text = ~label,
                    xanchor = xanchor,
                    xshift = 3,
                    font = list(color = "rgba(0,0,0,1)"),
                    bgcolor = "rgba(255,255,255,1)",
                    showarrow = FALSE,
                    showlegend = FALSE) 
}

order_y <- function(p, orderby){ # orderby uses tidyselect
  df <- plotly_data(p)
  ord <- arrange(df, {{ orderby }}) %>% dplyr::select(species) %>% unlist()
  p %>%
    layout(yaxis = list(categoryorder = "array", 
                        categoryarray = ~ord))
}

species_plotly_top10 <- function(df, showerrorbars = TRUE){
  set.seed(1)
  df <- topnrows(df, 10, "value")
  df$label <- paste0("", round(df$value * 100, 0), "%")
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  ord <- arrange(df, value) %>% dplyr::select(species) %>% unlist()
  p <- plot_ly_yinside(df) %>%
    fixed_layout() %>%
    add_tooltips() %>%
    order_y(value)
  if (showerrorbars){ # add error bars
    p <- p %>% add_error()
  }
  # add the values onto the bars
  p <- p %>% add_label_onleft()
  p
}
species_plotly_common <- species_plotly_top10

species_plotly_all_root <- function(df){
  traits <- get("traits", envir = globalenv())
  df <- dplyr::left_join(df, traits, by = c(species = "Common Name"))
  df$label <- paste0("", round(df$value * 100, 0), "%")
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  p <- plot_ly_youtside(df) %>%
    fixed_layout() %>%
    add_tooltips() 
  p
}

species_plotly_rel_all_root <- function(df){
  traits <- get("traits", envir = globalenv())
  df <- dplyr::left_join(df, traits, by = c(species = "Common Name"))
  df$label <- paste0("", round(df$value * 100, 0), "%")
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  p <- plot_ly_youtside(df, log2 = TRUE) %>%
    fixed_layout() %>%
    add_tooltips() %>%
    add_label_onsignside()
  p
}

# species_plotly_all_root(df) %>%
#   order_y(upper) %>%
#   add_error() %>%
#   add_label_onright()

species_plotly_different <- function(df){
  set.seed(1)
  df <- topnrows(df, 10, "value")
  df$label <- paste0("x ", round(df$value, 2))
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  plot_ly_specroot(df) %>%
  # add the values onto the bars
  add_annotations(x  = ~log10(value), 
                  y = ~species, 
                  text = df$label,
                  xanchor = "right",
                  xshift = -3,
                  bgcolor = "rgba(255,255,255,1)",
                  showarrow = FALSE,
                  showlegend = FALSE) %>%
  # alter order
  plotly::layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed"),
                 xaxis = list(type = "log")) 
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
