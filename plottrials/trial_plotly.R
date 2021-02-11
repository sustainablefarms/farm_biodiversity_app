model_data <- load_model_data()
new_data_mean <- get_new_data_mean(model_data)
current_values <- isolate(reactiveValuesToList(readRDS("./current_values_one_patch.rds")))
Xocc <- newXocc_fromselected(current_values)
species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                     Xocc)
df <- topnrows(tocommon(species_prob_current), 10, "value")
df$label <- paste0("", round(df$value * 100, 0), "%")

df$tooltip <- paste0(df$species, " has some interest features.")

traits <- read.csv("../sflddata/private/data/raw/Australian_Bird_Data_Version_1.csv", stringsAsFactors = FALSE)
pltdata <- traits %>%
  dplyr::filter(X3_Taxon_common_name_2 %in% df$species) %>%
  dplyr::select(`Common Name` = X3_Taxon_common_name_2,
                `Scientific Name` = X7_Taxon_scientific_name_CandB_2, 
                `Body Length` = X96_Body_length_8,
                `Body Mass` = X99_Body_mass_average_8) %>%
  dplyr::mutate(`Body Length` = as.numeric(`Body Length`),
                `Body Mass` = as.numeric(`Body Mass`)) %>%
  dplyr::right_join(df, by = c(`Common Name` = "species"))

colnames(pltdata)[[1]] <- "species"


pltdata %>%
  arrange(`Body Length`) %>%
  plot_ly_specroot() %>%
  layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed"))

plot_ly_specroot(df) %>%
  layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed"))

plt1 <- plot_ly_specroot(df) %>%
  # add error bars
  style(error_x = list(visible = TRUE,
                        type = 'data',
                            array = df$upper - df$value,
                            arrayminus = df$value - df$lower,
                            symmetric = FALSE,
                            color = '#000000')) %>%
  # add the values onto the bars
  add_annotations(x  = ~lower, 
                  y = ~species, 
                  text = df$label,
                  xanchor = "right",
                  xshift = -3,
                  bgcolor = "rgba(255,255,255,1)",
                  showarrow = FALSE,
                  showlegend = FALSE)
plt2 <- plot_ly_specroot(df) %>%
  # alter order
  layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed")) 
subplot(plt1, plt2) %>%
  config(displayModeBar = FALSE)

