# function to plot species barcharts
species_ggplot <- function(df, title = "", add_plus = FALSE, errorbar = FALSE){
  set.seed(1)
  df <- df[order(df$value), ]
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
    geom_bar(stat = "identity")
  if (errorbar){
    plot <- plot + geom_errorbar(aes(ymin = lower, ymax = upper),
                                 width = 0.3)
  }
  plot <- plot +
    geom_text(aes(y = 0, label = paste0("  ", species)),
      size = 4, color = "white", hjust = 0) +
    geom_text(aes(y = value, label = label, color = value),
      size = 4, hjust = 0) +
    coord_flip(clip = "off") +
    scale_x_discrete(limits = levels(df$species)) +
    scale_y_continuous(expand = c(0, 0)) +
    expand_limits(y = c(0, max(df$value) * 1.1)) +
    theme_void() +
    ggtitle(title) +
    theme(legend.position = "none")
  

  return(plot)
}

species_ggplotInModal <- function(model_data, current_values, new_data_mean){
  newXocc <- newXocc_fromselected(current_values)
  prediction_current_wlimits = data.frame(msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                                       newXocc))
  prediction_current_wlimits$species = rownames(prediction_current_wlimits)
  # prediction_of_mean = data.frame(msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
  #                                                                              new_data_mean))
  # prediction_of_mean$species = rownames(prediction_of_mean)
  
  traits <- read.csv("../sflddata/private/data/raw/Australian_Bird_Data_Version_1.csv", stringsAsFactors = FALSE)
  pltdata <- traits %>%
    dplyr::filter(X3_Taxon_common_name_2 %in% model_data$species) %>%
    dplyr::select(`Common Name` = X3_Taxon_common_name_2,
                  `Scientific Name` = X7_Taxon_scientific_name_CandB_2, 
                  `Body Length` = X96_Body_length_8,
                  `Body Mass` = X99_Body_mass_average_8) %>%
    dplyr::mutate(`Body Length` = as.numeric(`Body Length`),
                  `Body Mass` = as.numeric(`Body Mass`)) %>%
    dplyr::right_join(prediction_current_wlimits, by = c(`Common Name` = "species"))
  
  reorder(pltdata$`Common Name`, pltdata$`Body Length`)
  
  # pltdata <- dplyr::left_join(pltdata, prediction_of_mean[, c("species", "median")], by = c(`Common Name` = "species"), suffix = c("", ".m")) %>%
  #   dplyr::mutate(ratio = median / median.m)
  
  plt <- pltdata %>%
    dplyr::mutate(`Common Name` =
                    reorder(as.factor(`Common Name`), `Body Length`)) %>%
    dplyr::arrange(`Common Name`) %>%
    ggplot(aes(x = `Common Name`, y = median, fill = median)) +
    # geom_text(aes(y = 0, label = paste0("  ", `Common Name`)),
    #           size = 1, color = "white", hjust = 0) +
    geom_bar(stat = "identity") +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.5) +
    # geom_text(aes(y = 0, label = paste0("  ", `Common Name`)),
    #           size = 1, color = "white", hjust = 0) +
    coord_flip(clip = "off") +
    # coord_polar() +
    scale_x_discrete(name = "Increasing Body Length ---->") +
    # scale_y_continuous(expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = expansion()) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          panel.grid = element_blank())
  return(plt)
}