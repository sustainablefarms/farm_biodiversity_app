model_data <- load_model_data()
new_data_mean <- get_new_data_mean(model_data)
current_values <- isolate(reactiveValuesToList(readRDS("./current_values_one_patch.rds")))
Xocc <- newXocc_fromselected(current_values)
species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                     Xocc)
df <- topnrows(tocommon(species_prob_current), 10, "value")
df$label <- paste0("", round(df$value * 100, 0), "%")

df$tooltip <- paste0(df$species, " has some interest features.")



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

