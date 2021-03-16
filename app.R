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


# load requisite packages
# devtools::install_github("https://github.com/sustainablefarms/msod", ref = "modelclasses")
pkgload::load_all(".")
# myapp()
# app_predictionsonly()
app_predictiondetailsonly()
