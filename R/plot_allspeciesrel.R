
plot_allspeciesrel <- function(spec_different){
  ## fun inputs
  traits <- get("traits", envir = globalenv())
  df_both <- dplyr::left_join(spec_different, traits, by = c(species = "Common Name"))
  
  df_both %>%
    dplyr::rename(`Common Name` = species) %>%
    ordfactby(`Common Name`, `Body Length`) %>%
    ggplot(aes(x = `Common Name`, y = value, fill = value)) +
    geom_hline(yintercept = 1) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip(clip = "off") +
    scale_x_discrete(name = "Increasing Body Length ---->") +
    scale_y_continuous(name = "Ratio", expand = expansion(), trans = "log10") +
    scale_fill_continuous(trans = "log10") +
    # scale_fill_distiller(palette = "BrBG", trans = "log10", direction = 1) +
    ggtitle("Relative Occupancy Probability") +
    theme_minimal() +
    theme(legend.position = "none")
}
