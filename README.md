
# Introduction
This is the code repository for running the Sustainable Farms bird occupancy estimator web app. The web app is built using R shiny. It has a backend of R and a front-end based on Bootstrap 3. The app is deployed on shinyapps.io.

# Running and Deploying the App
## Offline
1. Install a specific version of Kassel's 'msod' package.
```
install.packages("remotes")
remotes::install_github("https://github.com/sustainablefarms/msod", ref = "e22069c9cfd9e2c9e007396e97f0d2e22e088713")
```

2. Open `app.R` in Rstudio. Press `Run App` in the top right of the 'app.R' screen. Rstudio will detect packages that need installation. Alternately, install R packages listed as dependencies in the DESCRIPTION file, then run shiny::runApp().

Note that installing many R packages in one go can often have issues. This seems mostly due to the many packages required and the way R works. Keep trying though: getting things to install is usually an easy fix like restarting R, upgrading to a new version of R, and on Windows NOT installing CRAN packages from source.

## Deploy the App to shinyapps.io

1. Install 'msod'. There is one package 'msod', not listed in the DESCRIPTION file as a dependency. This is because it is only on github. It must be installed using `remotes::install_github()`, and for compatibility a particular git commit must be used. This allows `rsconnect` to correctly determine where to get it from when loading the app on shinyapps.io . To install 'msod' run the following:

```
install.packages("remotes")
remotes::install_github("https://github.com/sustainablefarms/msod", ref = "e22069c9cfd9e2c9e007396e97f0d2e22e088713")
```

See https://github.com/rstudio/rsconnect/issues/88 for more information on using custom package dependencies. Dependencies, as perceived by `rsconnect`, can be listed by `rsconnect::appDependencies()`.

2. Use `rsconnect` inside RStudio send the app to shinyapps.io . You will need the login information for our account on shinyapps.io See https://docs.rstudio.com/shinyapps.io for configuring R to use a shinyapps.io user account. 
Test that the app works on shinyapps.io by deploying to a development app name:

```
rsconnect::deployApp(appName = "birdbio_dev4")
```

If the app doesn't run as expected, logs kept by shinyapps.io can be accessed from the shinyapps.io (account dashboard)[https://www.shinyapps.io/admin/#/dashboard], these can help diagnose the problem when it isn't occurring when running the app offline.

Once the app is running correctly on shinyapps.io, deploy the app under the release name, which is currently 'bird_checker'.

```
rsconnect::deployApp(appName = "bird_checker")
```

