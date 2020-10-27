
test_that("poccupancy function works", {
  model_data <- readRDS("../../data/model_data.rds")
  new_data <- readRDS("new_data.rds")
  new_data$NMdetected <- as.numeric(new_data$NMdetected)
  prediction_current = as.numeric(msod::poccupancy_standalone_nolv(
    new_data,
    model_data$XoccProcess,
    model_data$u.b))
})
