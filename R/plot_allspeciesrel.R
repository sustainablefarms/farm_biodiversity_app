
plot_allspeciesrel <- function(spec_different){
  scipen <- getOption("scipen")
  options(scipen = 999)
  on.exit(options(scipen = scipen))
  ## fun inputs
  traits <- get("traits", envir = globalenv())
  df_both <- dplyr::left_join(spec_different, traits, by = c(species = "Common Name"))
  
  if (all(abs(df_both$value - 1) < 1E-3)){
      trans <- "identity"
  } else {
      trans <- "log2"
  }

  df_both %>%
    dplyr::rename(`Common Name` = species) %>%
    dplyr::mutate(`Common Name` = paste(`Common Name`, format(round(value, 2), digits = 2, scientific = FALSE))) %>%
    ordfactby(`Common Name`, `Body Length`) %>%
    ggplot(aes(x = `Common Name`, y = value, fill = value, 
               linetype = value > 1)) +
    geom_hline(yintercept = 1) +
    geom_bar(stat = "identity", show.legend = FALSE, color = "black") +
    coord_flip(clip = "off") +
    scale_x_discrete(name = "Increasing Body Length ---->") +
    scale_y_continuous(name = "Ratio", 
                       trans = trans) +
    # scale_fill_continuous(trans = trans) +
    scale_linetype_manual(values = c(3, 0), limits = c(FALSE, TRUE)) + 
    scale_fill_gradient(aesthetics = "fill",
                        trans = trans,
                        low = appcolors[["Green 10"]],
                        high = appcolors[["Dark Green"]]) +
    # scale_fill_distiller(palette = "BrBG", trans = "log10", direction = 1) +
    ggtitle("Relative Occupancy Probability") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(fill = NA, color = "grey"))
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
