#plot fun

plot_allspeciesprob <- function(species_prob_current){
  species_prob_current <- data.frame(species = rownames(species_prob_current), species_prob_current)
  traits <- get("traits", envir = globalenv())
  species_prob_current <- dplyr::left_join(species_prob_current, traits, by = c(species = "Common Name")) %>%
    dplyr::rename(`Common Name` = species)
  
  species_prob_current %>%
    ordfactby(`Common Name`, `Body Length`) %>%
    ggplot(aes(x = `Common Name`, y = median, fill = median)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
    coord_flip(clip = "off") +
    scale_x_discrete(name = "Increasing Body Length ---->") +
    scale_y_continuous(name = "Occupancy Probability", limits = c(0, 1), expand = expansion()) +
    ggtitle("Probability of Occupying at Least One Patch") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank())
}
