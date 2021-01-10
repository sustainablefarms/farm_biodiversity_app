# function to plot species barcharts
species_ggplot <- function(df, title = "", add_plus = FALSE, errorbar = FALSE){
  df$species <- factor(
    seq_len(10),
    levels = seq_len(10),
    labels = df$species)
  if(add_plus){
    df$label <- paste0("  +", round(df$value * 100, 0), "%")
  }else{
    df$label <- paste0("  ", round(df$value * 100, 0), "%")
  }

  plot <- ggplot(df,
    aes(x = species, y = value, fill = value)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = 0, label = paste0("  ", species)),
      size = 4, color = "white", hjust = 0) +
    geom_text(aes(y = value, label = label, color = value),
      size = 4, hjust = 0) +
    coord_flip(clip = "off") +
    scale_x_discrete(limits = rev(levels(df$species))) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y = c(0, max(df$value) * 1.1)) +
    theme_void() +
    ggtitle(title) +
    theme(legend.position = "none")
  
  if (errorbar){
    plot <- plot + geom_errorbar(aes(ymin = lower, ymax = upper))
  }

  return(plot)
}