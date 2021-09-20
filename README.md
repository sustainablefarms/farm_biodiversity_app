# README
<!-- vim-markdown-toc GFM -->

* [Introduction](#introduction)
* [Running and Deploying the App](#running-and-deploying-the-app)
	* [Offline](#offline)
	* [Deploy the App to shinyapps.io](#deploy-the-app-to-shinyappsio)
* [Reactive Flow](#reactive-flow)
* [Files and Directories](#files-and-directories)
* [Testing](#testing)
* [Running Modules](#running-modules)
* [Shiny Options](#shiny-options)
* [Google Analytics Tracking](#google-analytics-tracking)

<!-- vim-markdown-toc -->
## Introduction
This is the code repository for running the Sustainable Farms bird occupancy estimator web app. The web app is built using R shiny. It has a backend of R and a front-end based on Bootstrap 3. The app is deployed on shinyapps.io.

## Running and Deploying the App
### Offline
1. Install a specific version of Kassel's 'msod' package.
```
install.packages("remotes")
remotes::install_github("https://github.com/sustainablefarms/msod", ref = "e22069c9cfd9e2c9e007396e97f0d2e22e088713")
```

2. Open `app.R` in Rstudio. Press `Run App` in the top right of the 'app.R' screen. Rstudio will detect packages that need installation. Alternately, install R packages listed as dependencies in the DESCRIPTION file, then run shiny::runApp().

Note that installing many R packages in one go can often have issues. This seems mostly due to the many packages required and the way R works. Keep trying though: getting things to install is usually an easy fix like restarting R, upgrading to a new version of R, and on Windows NOT installing CRAN packages from source.

### Deploy the App to shinyapps.io

1. Install 'msod'. There is one package 'msod', not listed in the DESCRIPTION file as a dependency. This is because it is only on github. It must be installed using `remotes::install_github()`, and for compatibility a particular git commit must be used. This allows `rsconnect` to correctly determine where to get it from when loading the app on shinyapps.io . To install 'msod' run the following:

```
install.packages("remotes")
remotes::install_github("https://github.com/sustainablefarms/msod", ref = "e22069c9cfd9e2c9e007396e97f0d2e22e088713")
```

See https://github.com/rstudio/rsconnect/issues/88 for more information on using custom package dependencies. Dependencies, as perceived by `rsconnect`, can be listed by `rsconnect::appDependencies()`.

2. Use `rsconnect` inside RStudio send the app to shinyapps.io . You will need the login information for our account on shinyapps.io See https://docs.rstudio.com/shinyapps.io for configuring R to use a shinyapps.io user account. 
Test that the app works on shinyapps.io by deploying to a development app name:

```
rsconnect::deployApp(appName = "birdbio_dev4", appFileManifest = "./data/filemanifest.txt", logLevel = "verbose")
```

If the app doesn't run as expected, logs kept by shinyapps.io can be accessed from the shinyapps.io (account dashboard)[https://www.shinyapps.io/admin/#/dashboard], these can help diagnose the problem when it isn't occurring when running the app offline.

Once the app is running correctly on shinyapps.io, deploy the app under the release name, which is currently 'bird_checker'.

```
rsconnect::deployApp(appName = "bird_checker", appFileManifest = "./data/filemanifest.txt")
```

## Reactive Flow
The broad reactive flow of the whole app is:
location selected -> rainfall selected/update + patch attributes -> list of values (cval) -> predictions

## Files and Directories
Files in the root directory:
 + app.R contains the call to run the main app
 + DESCRIPTION a file that describes the repository as if it is an R package. This is useful for `rsconnect` and deploying the shinyapps.io
 + README.md this file
 + _disable_autoload.R The presence of this file is meant to turn off shiny's autoload feature. I'm not sure if it works.
 + todo.md is a to do list
 + .gitignore  A file for using .git

Directories:
 + R/ Contains functions for the app to build and run. Includes UI and Server shiny app functions, functions for creating small parts of the UI, full modules and more.
   + `main` contains the main UI and server function for the app.
   + `module_` files contain individual modules used in the app, most also contain a function for running the modules in standalone (very useful for development).
   + `UI_` files contain functions or objects for creating purely UI components in the app
   + `plot_` files contain function for plotting (which are typically then used in a outputPlot or outputPlotly call)
   + `prep_` files are used for preparing or loading data, usually at the start of the app
   + `serverhelp_` files are used by R in the server functions to help obtain, compute, sort, and modify covariate values or prediction information. `compute_prediction_data` does the core work of generating occupancy probability estimates
   + `_disable_autoload.R` is an empty file for telling R shiny to stop autoloading - I don't think it worked though.


 + data/ Stores various data for making and presenting results.
   + birdinfotable.rds is a precompiled bird info table. Prepared by calling `prep_birdinfotable()`.
   + consstatus.rds is a table of the 5 species of conservation concern in the model
   + model_data.rds is the parameters of the model, extracted from an `msod` fitted model using the script `build_model_data.R`
   + nicecovarnames.csv is a table of nicely readable covariate names for the abbreviated names used in the code
   + report.Rmd and similar is the template for the downloadable report
   + sa2_points_climate.rds is a table of the location points with their climate data. Built from sa2_points.rds with the script `update_sa2_worldclim_data.R`
   + sa2_polygons.rds Spatial polygons of each region represented by the points.
   + species_alaimages.csv An unused table giving suggested images from the Atlas of Living Australia to use for each bird. BirdLife Australia photographers tended to produce more eye catching images, so I have used BirdLife photographs instead.
   + species_birdlifeimgs.csv Table of BirdLife Australia photographs for each species. Used for reading in the images (which are saved in www/ for access reasons), printing the correct attribution and linking to the full image on birdlifephotography.org.au
   + species_shortstory.csv For each species a short sentence or two by Kassel Hingee summarising non-climate loadings for each species. These summaries are used as tooltips in the plotly species summaries.
   + state_borders.rds is a data set of state border vectors used to give context to the location. Prepared using `prepare_state_borders.R`
   + test-current_values_2patches.rds and test-predictions_data_2patches.rds contain covariate values and corresponding species predictions. They are used for testing modules and for experimenting with knitting report.Rmd
   + traits.rds Prepared by `preptraits()`, contains traits of each of the bird species. Used, for example, to order species by size in some plots.

+ scripts/ Contains R scripts for preparing various pieces of data (in data/) for the web app. I think `sa2_info.R` does not run fully in its current form. A demonstration of a redirection app (for if the app url changes) is included in `redirect_app.R`.

+ tests/
  + shinytest/ Contains a directory of snapshot tests for `shinytest`. Run these using `shinytest::testApp()`
  + testthat/ Obsolete `testthat` tests, primarily used for testing plotting, but now that shinytest is working they aren't needed.

+ www/ Stores files for the web app to access during execution. Mostly this is images in high and low res form. A (messy) collection of css styles used in the app is in base.css.

## Testing
Snapshot testing using `shinytest` tests all the main feature of the app, and how it looks. At commit 113d5b5a135d490d83484cd31bc890246c65533c on Jul 28, 2021, these tests passed except for tiny aesthetic differences AND the downloaded pdf report (which is always different due to the timestamp in it).

Take care with `predict_goulburn_1patch_moredetails.R` as the comparison of the downloaded pdf breaks the `shinytest` viewing functions. I've been getting away with looking at the .png snapshots in the corresponding expected and current directories.

## Running Modules 
Most modules can be run stand alone with nearly all features intact (references for the predictions module is an exception). Functions for running these modules as a shiny app are in the same .R file as the other module functions. For example the predictionsdetail module can be run with the function below, which is in `module_predictionsdetail.R`

```
app_predictiondetails()
```


## Shiny Options
The main `app.R` file in the root directory contains some options for running shiny.

```
options(
  shiny.launch.browser = FALSE,
  shiny.port = 7214,
  shiny.testmode = FALSE
)
```

The first two options stops shiny from launching a browser window, and makes sure shiny always hosts pages visible to the port 7214.
The option `shiny.testmode` is for testing purposes. Set to `TRUE` to get buttons that allows downloading covariate values and prediction values from the app (and maybe more things).


## Google Analytics Tracking
The app includes Google Analytics tracking for the activities in the following table. 
Event Name and Event Category should both be available to Google Analytics. 
Selected region, longitude and latitude are *not* tracked.

|Description   | Event Name | Event Category |
|--------------|------------|----------------|
|View more detail| viewmoredetail | button |
|Download the csv| downloadcsv | download |
Download the report| downloadreport | download
Update reference| updatereference | button
Switch off/on default reference | switchreference | switch, value is FALSE when default turned off (I think).
Request to get woody canopy from WALD's satellite data | getlatlonfromsatellite | button
When new predictions made | predictionsmade | server
Click a link to birdlife photography | link_birdlifephotography | outbound_link
Click a link to a birdlife profile page | link_birdlifeprofile | outbound_link
Click a link to WALD's tree map viewing page | link_waldtree | outbound_link


Settings for this are all in www/google-analytics.html. 
It uses gtag.js (hence the use of the function gtag rather than ga). 
The current ID for tracking is for the release version. 
The development version has a different measurement ID: G-C7HN26FLS5


