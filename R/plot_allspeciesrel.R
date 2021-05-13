
plot_allspeciesrel <- function(spec_different){
  ## fun inputs
  traits <- get("traits", envir = globalenv())
  df_both <- dplyr::left_join(spec_different, traits, by = c(species = "Common Name"))
  
  df_both %>%
    dplyr::rename(`Common Name` = species) %>%
    dplyr::mutate(`Common Name` = paste(`Common Name`, format(value, digits = 2))) %>%
    ordfactby(`Common Name`, `Body Length`) %>%
    ggplot(aes(x = `Common Name`, y = value, fill = value)) +
    geom_hline(yintercept = 1) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip(clip = "off") +
    scale_x_discrete(name = "Increasing Body Length ---->") +
    scale_y_continuous(name = "Ratio", expand = expansion(), trans = "log2") +
    scale_fill_continuous(trans = "log2") +
    # scale_fill_distiller(palette = "BrBG", trans = "log10", direction = 1) +
    ggtitle("Relative Occupancy Probability") +
    theme_minimal() +
    theme(legend.position = "none")
}

# breakfun <- function(dmin, dmax){
#   breaks <- vector()
#   if (dmin < 1){# points that 1/1, 1/2, 1/4, 1/8 etc , 2^-1, 2^-2, 2^-3 etc until 2^-x ~ dmin <==> log2(dmin)==
#     seq(1, )
#     by = (1/dmin - 1)
#     1/seq(1/dmin, 1, by = -2)
#   }
#   
#   
#   
#   10^(seq(log10(dmin), 0, by = 0.2))
#   seq(dmin, 1, by = 0.1)
# }