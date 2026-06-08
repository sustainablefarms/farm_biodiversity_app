# See README for more on running and deploying this app
options(
  shiny.launch.browser = FALSE,
  shiny.port = 7214,
  # shinytest2 forces options(warn = 2) for dev-package apps (app.R load_all()s
  # the package), which turns compute_richness()'s benign warning into a fatal
  # error and breaks the predictions screen under the visual-test harness.
  # warn = 1 prints warnings as they occur but never escalates them. See README.
  warn = 1
)
pkgload::load_all("."); myapp()

