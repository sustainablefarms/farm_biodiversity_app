#' @title ggplot richness plot
#' @examples
#' datar <- compile_predictions(readRDS("./current_values.rds"),
#'                     species_prob_mean,
#'                     refisaverage = TRUE)
#' richness_plot(datar$species_richness)

richness_plot <- function(species_richness, labeltextsize = 20, labelnudge = -1){
  textcolours <- dplyr::case_when(
    species_richness$E < mean(species_richness$E) ~ "#026666", 
    TRUE ~ "#FFFFFF")
  names(textcolours) <- species_richness$category
  
  plot <- ggplot(species_richness, aes(x = category, y = E, fill = E)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = category, y = E,
                label = formatC(E, digits = 0, format = "f"),
                colour = category),
                hjust = 1,
                nudge_y = labelnudge,
                show.legend = FALSE,
                size = labeltextsize) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(
    y = c(0, max(25, species_richness$E * 1.1))) +
  scale_x_discrete(position = "bottom") +
  scale_color_discrete(type = textcolours) + 
  scale_fill_gradient(aesthetics = "fill",
                      low = "#d0e7f4",
                      high = "#178BCA") +
  # geom_errorbar(aes(ymin = E - 2 * sqrt(V), ymax = E + 2 * sqrt(V)), width = 0.2) +
  coord_flip() +
  ggtitle(NULL) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12, hjust = 0), #hjust arranges labels to be left justified
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey90"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_rect(color = "grey90", fill = NA)
  )
  return(plot)
}
