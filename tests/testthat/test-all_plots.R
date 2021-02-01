# test that all plots can be built
devtools::load_all()

test_that("prediction plots for Gundagai", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  current_values <- list(AnnMeanTemp = NULL,
                         AnnPrec = 704,
                         AnnTempRange = NULL,
                         MaxTWarmMonth = NULL,
                         MinTColdMonth  = NULL,
                         PrecSeasonality = 15,
                         PrecWarmQ = NULL,
                         latitude = NULL,
                         midstorey = 6,
                         noisy_miner = TRUE,
                         patches = 1,
                         woody_veg = 8,
                         year = 2018)
  points <- readRDS("data/sa2_points_climate.rds")
  preddata <- compute_prediction_data(model_data, current_values, new_data_mean,
                                      points, selected_region)
  richness_plot(preddata$species_richness)
  species_ggplot(
    df = preddata$species_predictions$common,
    title = "Most likely species",
    add_plus = FALSE,
    errorbar = TRUE)
  species_ggplot(
    df = preddata$species_predictions$different,
    title = "Locally prevalent species",
    add_plus = TRUE)
})

