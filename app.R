# See README for more on running and deploying this app
options(
  shiny.launch.browser = FALSE,
  shiny.port = 7214,
  shiny.testmode = FALSE
)
pkgload::load_all("."); myapp()
# pkgload::load_all("."); app_selectpatchonly()
# pkgload::load_all("."); app_selectlocationonly()
# pkgload::load_all("."); app_predictionsonly()
# pkgload::load_all("."); app_predictiondetailsonly()
# pkgload::load_all("."); app_patchattronly()
