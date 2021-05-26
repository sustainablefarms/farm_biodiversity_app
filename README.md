# farm_biodiversity_app
Shiny app displaying results of the Sustainable Farms 'linking data' project. To run this app offline download the full repository. Then install a specific version of Kassel's 'msod' package.
```
install.packages("remotes")
remotes::install_github("https://github.com/sustainablefarms/msod", ref = "e22069c9cfd9e2c9e007396e97f0d2e22e088713")
```

Then open `app.R` in Rstudio. Press `Run App` in the top right of the 'app.R' screen. Rstudio will detect packages that need installation.

Installing the R packages can run into conflicts and errors. This seems mostly due to the many packages required and the way R works. Keep trying though: getting things to install is usually an easy fix like restarting R, upgrading to a new version of R, and on window NOT installing CRAN packages from source.

