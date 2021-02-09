model_data <- load_model_data()
new_data_mean <- get_new_data_mean(model_data)
selected_region <- "Gundagai"
current_values <- isolate(reactiveValuesToList(readRDS("./current_values_two_patches.rds")))
preddata <- compute_prediction_data(model_data, current_values, new_data_mean)
data <- preddata$species_predictions$common
library(plotly)
data$tooltip <- paste0(data$species, " has some interest features.")
plot_ly(data = data,
        y = ~species,
        x = ~value,
        color = ~value,
        marker = list(colorscale = "Blues", #"[[0, 'rgb(0,0,255)'], [1, 'rgb(14, 17, 60)']]", 
                      reversescale = TRUE, cmax = 1.2), #cmin = 0, cmax = 1),
        # text = paste0(round(data$value, digits = 2) * 100, "%"),
        text = data$tooltip,
        # hoverinfo = 'text',
        # textposition = 'auto',
        hovertemplate = paste('<b>%{text}</b><extra></extra>'), #the <extra></extra> removes the 'trace 0' extra information
        error_x = ~list(type = 'data',
          array = upper - value,
          arrayminus = value - lower,
          symmetric = FALSE,
          color = '#000000'),
        type = "bar") %>%
  add_annotations(x  = ~value, 
                  y = ~species, 
                  text = ~paste0(round(value, digits = 2) * 100, "%"),
                  showarrow = FALSE) %>%
  layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed")) %>%
  hide_colorbar()

plot_ly(data = data,
        y = ~species,
        x = ~value,
        text = paste0(round(data$value, digits = 2) * 100, "%"),
        hovertemplate = paste('<b>%{text}</b>'),
        type = "bar") %>%
  layout(yaxis = ~list(categoryorder = "array", categoryarray = value, autorange = "reversed")) %>%
  hide_colorbar()
