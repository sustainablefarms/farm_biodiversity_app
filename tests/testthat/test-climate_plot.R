# test climate plot
test_that("test plot of AnnPrec for Gundagai", {
  data <- readRDS("data/sa2_points_climate.rds")
  variable  <- "AnnPrec"
  region <- "Gundagai"
  title <- "test climate plot"
  climate_plot(data, variable, region, title)
})

test_that("test plot of Maximum Temp for Gundagai", {
  data <- readRDS("data/sa2_points_climate.rds")
  variable  <- "MaxTWarmMonth"
  region <- "Gundagai"
  title <- "test climate plot"
  climate_plot(data, variable, region, title)
})

