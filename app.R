# See README for more on running and deploying this app
options(
  shiny.launch.browser = FALSE,
  shiny.port = 7214
)
pkgload::load_all("."); myapp()

