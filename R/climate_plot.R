climate_plot <- function(data, variable, region, title = ""){
  ggplot(data, aes_string(x = variable, y = 1)) +
    geom_quasirandom(
      data = data[data$label != region, ],
      size = 2, groupOnX = FALSE, color = "#4e839c") +
    geom_quasirandom(
      data = data[data$label == region, ],
      size = 4, groupOnX = FALSE, color = "black") +
    theme_bw() +
    ggtitle(title) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, size = 10),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor.x = element_line(color = "grey80"),
      panel.grid.major.x = element_line(color = "grey80"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_blank()
    )
}