climate_modal <- function(ns, ...){
  showModal(
    modalDialog(
      plotOutput(ns("climate_plot")),
      ...,
      "Each of the regions possible to select for this app is represented by a dot.",
      "The large dot is the value for your region,",
      "which is the worldclim estimate for the centre of your region.",
      fade = TRUE,
      easyClose = TRUE
    )
  )
}
