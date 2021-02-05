climate_modal <- function(ns){
  showModal(
    modalDialog(
      plotOutput(ns("climate_plot")),
      footer = NULL,
      easyClose = TRUE
    )
  )
}