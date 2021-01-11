# richness plot

richness_plot <- function(species_richness){
  plot <- ggplot(species_richness, aes(x = category, y = E, fill = category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(
    y = c(0, max(species_richness$E + 2 * sqrt(species_richness$V)) + 3)) +
  scale_x_discrete(position = "top") +
  scale_discrete_manual(aesthetics = "fill", values = c("#81a2b3", "#4e839c", "#81a2b3")) +
  geom_errorbar(aes(ymin = E - 2 * sqrt(V), ymax = E + 2 * sqrt(V)), width = 0.2) +
  coord_flip() +
  ggtitle("Number of bird species") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.ticks.y = element_blank(),
        panel.grid.minor.x = element_line(color = "grey80"),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "grey90", colour = NA),
        panel.border = element_blank()
  )
  return(plot)
}