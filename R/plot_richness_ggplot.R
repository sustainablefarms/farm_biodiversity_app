#' @title ggplot richness plot
#' @examples
#' datar <- compile_predictions(readRDS("./current_values.rds"),
#'                     species_prob_mean,
#'                     refisaverage = TRUE)
#' richness_plot(datar$species_richness)

richness_plot_root <- function(species_richness){
  ggplot(species_richness, aes(x = category, y = E, fill = E)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(
    y = c(0, max(25, species_richness$E * 1.1))) +
  scale_fill_gradient(aesthetics = "fill",
                      low = appcolors[["Green 10"]],
                      high = appcolors[["Dark Green"]]) +
  # geom_errorbar(aes(ymin = E - 2 * sqrt(V), ymax = E + 2 * sqrt(V)), width = 0.2) +
  coord_flip() +
  ggtitle(NULL)
}

richness_plot <- function(species_richness, labeltextsize = 20, labelnudge = -1){
  textcolours <- dplyr::case_when(
    species_richness$E < mean(range(species_richness$E)) ~ "#026666", 
    TRUE ~ "#FFFFFF")
  names(textcolours) <- species_richness$category
  
  plot <- richness_plot_root(species_richness) +  
  scale_x_discrete(labels = function(x) gsub('(.{1,15})(\\s|$)', '\\1\n', x), #15 characters long and leaves a new line at the end
  position = "bottom") +
  geom_text(aes(x = category, y = E,
                label = formatC(E, digits = 0, format = "f"),
                colour = category),
                hjust = 1,
                fontface = "bold",
                nudge_y = labelnudge,
                show.legend = FALSE,
                size = labeltextsize) +
  scale_color_discrete(type = textcolours) + 
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12, hjust = 0,
                                   colour = appcolors[["Dark Green"]]), #hjust arranges labels to be left justified
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = NA, colour = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        panel.border = element_blank()
  )
  return(plot)
}

richness_plot_pdf <- function(species_richness){
  richness_plot_root(species_richness) + 
    scale_x_discrete(position = "bottom") +
    geom_text(aes(x = category, y = E,
                  label = formatC(E, digits = 1, format = "f")),
              hjust = 0,
              nudge_y = 0.1,
              show.legend = FALSE) +
    theme_minimal() + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.y = element_text(size = 12, hjust = 0), #hjust arranges labels to be left justified
          axis.ticks.y = element_blank(),
          # panel.grid = element_blank(),
          panel.grid.minor.x = element_blank(),
          # panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.background = element_rect(fill = NA, colour = NA),
          # panel.background = element_rect(fill = NA, colour = NA),
          panel.border = element_rect(fill = NA, color = "grey")
    )
}
