plot_ly_youtside <- function(df, log2 = FALSE){
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  if (log2){
    df$value <- log2(df$value)
    df$pattern_shape <- dplyr::case_when(
      df$value >= 0 ~ "",
      TRUE ~ "x")
  }
  
  plt <- plot_ly() %>%
    add_trace_colorbarsbyvalue(df)
  plt %>%
    plotly::layout(
      yaxis = list(title = "", visible = TRUE, type = "category",
                   color = appcolors[["Dark Green"]])
    )
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
                  font = list(color = appcolors[["Dark Green"]]),
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
                    font = list(color = appcolors[["Dark Green"]]),
                    bgcolor = "rgba(255,255,255,0)",
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
                    font = list(color = appcolors[["Dark Green"]]),
                    bgcolor = "rgba(255,255,255,0)",
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

# plotly vline as per https://stackoverflow.com/questions/34093169/horizontal-vertical-line-in-plotly/34097929#34097929
plotlyvline <- function(x = 0, color = "black") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    layer = "below",
    line = list(color = color,
                dash = "dot",
                width = 1)
  )
}

##### Functions Called in App #####
prob_top10 <- function(df, showerrorbars = TRUE){
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
  p <- p %>% add_label_onright()
  p
}


all_prob <- function(df){
  traits <- get("traits", envir = globalenv())
  df <- dplyr::left_join(df, traits, by = c(species = "Common Name"))
  df$label <- paste0("", round(df$value * 100, 0), "%")
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  p <- plot_ly_youtside(df) %>%
    fixed_layout() %>%
    add_tooltips() 
  p
}

all_rel <- function(df){
  traits <- get("traits", envir = globalenv())
  df <- dplyr::left_join(df, traits, by = c(species = "Common Name"))
  df$label <- paste0(formatC(df$value, format = "fg", 2))
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  p <- plot_ly_youtside(df, log2 = TRUE) %>%
    fixed_layout() %>%
    add_tooltips() %>%
    add_label_onsignside()
  p %>%
    plotly::add_annotations(text = " ", #this creates the right size of empty space above plot
                            showarrow = FALSE,
                            x=0, xanchor = "left",
                            y=nrow(df)) %>%
    plotly::add_annotations(text = "More likely in S.2-->",
                            showarrow = FALSE,
                            font = list(family = "Inter",
                                        color = appcolors[["Dark Green"]]),
                            x=0, xanchor = "left",
                            y=1, yref = "paper") %>%
    plotly::add_annotations(text = "<-- More likely in S.1",
                            showarrow = FALSE,
                            font = list(family = "Inter",
                                        color = appcolors[["Dark Green"]]),
                            x=0, xanchor = "right",
                            y=1, yref = "paper") %>%
    plotly::layout(shapes = list(plotlyvline(0)))
}

plot_ly_youtside_adj <- function(df){
  traits <- get("traits", envir = globalenv())
  df <- dplyr::left_join(df, traits, by = c(species = "Common Name")) %>%
    select(-value) %>% #for this table the value is the ratio
    tidyr::pivot_longer(c(value.cur, value.ref), names_to = "scenario", values_to = "value")
  df$label <- paste0(formatC(df$value, format = "fg", 2))
  df$tooltip <- speciesinfo[df$species, "shortstory"]
  pal <- defaultpal(df$value)
  plt <- plot_ly() %>%
    add_trace_colorbarsbyvalue(data = df %>% dplyr::filter(scenario == "value.cur"), pal = pal) %>%
    style(
          #hoverinfo = TRUE, #formats the tooltips overridden by hovertemplate
          hoverlabel = list(bgcolor = "white",
                            font = list(color = "black",
                                        size = 12)),
          hovertemplate = paste('S2: %{hovertext}<extra></extra>')
    ) %>% 
    add_trace_colorbarsbyvalue(data = df %>% dplyr::filter(scenario == "value.ref"), pal = pal) %>%
    style( traces = 2,
              # width = 0.2,
              # type = "bar",  #make a bar plot
              # y = ~species,
              # x = ~value,
              marker.line.width = 2, #shortcut updates
              marker.color = "#FFFFFF",
              showlegend = FALSE,
          hoverinfo = TRUE, #formats the tooltips
          hovertext = ~label, #from provided data
          hoverlabel = list(bgcolor = "white",
                            font = list(color = "black",
                                        size = 12)),
          hovertemplate = paste('S1: %{hovertext}<extra></extra>')
    )

  plt %>%
    plotly::layout(
      yaxis = list(title = "", visible = TRUE, type = "category",
                   color = appcolors[["Dark Green"]]),
      barmode = "group",
      bargap = 0.5
    ) %>%
    fixed_layout() #removes buttons and things
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
