devtools::load_all()

test_that("prediction plots for Gundagai and a single patch using plotly", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  current_values <- isolate(reactiveValuesToList(readRDS("./current_values_one_patch.rds")))
  preddata <- compute_prediction_data(model_data, current_values, new_data_mean)
  richness_plot(preddata$species_richness)
  species_plotly(
    df = preddata$species_predictions$common,
    title = "Most likely species",
    add_plus = FALSE,
    errorbar = TRUE)
  species_plotly(
    df = preddata$species_predictions$different,
    title = "Locally prevalent species",
    add_plus = TRUE)
})