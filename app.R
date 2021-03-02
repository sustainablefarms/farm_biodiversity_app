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
# rsconnect::deployApp(appName = "birdbio_dev3")
# rsconnect::terminateApp("birdbio_dev2")
# rsconnect::purgeApp("birdbio_dev2")


# load requisite packages
# devtools::install_github("https://github.com/sustainablefarms/msod")
pkgload::load_all(".")
library(msod)
library(sf)
library(shiny)
library(shinythemes)
library(plotly)
library(Rfast)
library(ggbeeswarm)
library(ggplot2)
library(shinyBS)
library(tippy)

# Data Preparations
model_data <- load_model_data()
new_data_mean <- get_new_data_mean(model_data)

# preptraits(model_data)
loadtraits2global()
tempdir <- tempdir()
report_path <- paste0(tempdir, "/", "report.Rmd") #file location assumes host is a unix machine
stopifnot(file.copy("report.Rmd", report_path, overwrite = TRUE)) 

myapp()
