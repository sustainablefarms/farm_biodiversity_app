# Package functions and several tests read data via project-root-relative paths
# (e.g. load_model_data()'s default "data/model_data.rds",
# readRDS("data/sa2_points_climate.rds")), matching how the app runs. testthat runs each
# test file from tests/testthat/, and setwd() can't fix that (testthat's source_file resets
# the working directory per file). Bridge it with a temporary symlink
# tests/testthat/data -> ../../data so "data/..." resolves, removed again on teardown.
local({
  here   <- normalizePath(getwd(), winslash = "/")          # tests/testthat during setup
  link   <- file.path(here, "data")
  target <- normalizePath(file.path(here, "..", "..", "data"), mustWork = TRUE)
  if (!file.exists(link)) {
    file.symlink(target, link)
    withr::defer(unlink(link), teardown_env())
  }
})
