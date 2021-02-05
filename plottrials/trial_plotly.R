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
        text = paste0(round(data$value, digits = 2) * 100, "%"),
        textposition = 'auto',
        hovertemplate = paste('<i>Bird information for</i>: %species'),
        error_x = ~list(type = 'data',
          array = upper - value,
          arrayminus = value - lower,
          symmetric = FALSE,
          color = '#000000'),
        type = "bar") %>%
  layout(yaxis = ~list(categoryorder = "array", categoryarray = value))
