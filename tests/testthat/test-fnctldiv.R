devtools::load_all()

test_that("functonal diversity plot runs for single patch", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  points <- readRDS("data/sa2_points_climate.rds")
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
  plts <- functdivplot(model_data, current_values, points, selected_region)
  plts[[1]]
  plts[[2]]
})

test_that("functonal diversity plot runs for two patches", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  points <- readRDS("data/sa2_points_climate.rds")
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
  plts <- functdivplot(model_data, current_values, points, selected_region)
  plts[[1]]
  plts[[2]]
})