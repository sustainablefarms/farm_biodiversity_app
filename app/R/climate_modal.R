climate_modal <- function(){
  showModal(
    modalDialog(
      plotOutput("climate_plot"),
      footer = NULL,
      easyClose = TRUE
    )
  )
}