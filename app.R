# shiny app to interact with model outcomes from the linking data project
  # select a region
    # this gives climate data
  # set some parameters about the farm
    # presence of Noisy Miners
    # % woody veg
    # % midstorey cover
    # number of patches (?)
  # get prediction of
    # likely species (list)
    # richness (estimate)
    # what you could do to improve this

# This page is quite informative on using custom package dependencies: https://github.com/rstudio/rsconnect/issues/88
# rsconnect::appDependencies()
# rsconnect::deployApp(appName = "birdbio_dev4")
# rsconnect::terminateApp("birdbio_dev2")
# rsconnect::purgeApp("birdbio_dev2")
# rsconnect::deployApp(appName = "bird_checker")

# load requisite packages
# devtools::install_github("https://github.com/sustainablefarms/msod", ref = "e22069c9cfd9e2c9e007396e97f0d2e22e088713")
options(
  shiny.launch.browser = FALSE,
  shiny.port = 7214,
  shiny.testmode = TRUE
)
pkgload::load_all("."); myapp()
# pkgload::load_all("."); app_selectpatchonly()
# app_selectlocationonly()
# pkgload::load_all("."); app_predictionsonly()
# pkgload::load_all("."); app_predictiondetailsonly()
# 
# main_app_prep() # loads things into global environment, prepares report file
# shinyApp(ui, server)
# shinyAppDir(".")
