test_that("compute prediction data runs on Gundagai", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  current_values <- isolate(reactiveValuesToList(readRDS("./current_values_two_patches.rds")))
  points <- readRDS("data/sa2_points_climate.rds")
  preddata <- compute_prediction_data(model_data, current_values, new_data_mean)
})
