app_predictionsonly <- function(){
  main_app_prep()
  current_values <- do.call(reactiveValues, readRDS("tests/testthat/current_values_2patches.rds"))
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      fluidRow(predictionsUI("pred")),
      theme = bslib::bs_theme(version = 3, "lumen"))
      },
           function(input, output, session){
             predictionsServer("pred", current_values,
                               model_data, new_data_mean,
                               report_path)
           })
}

app_selectpatchonly <- function(){
  main_app_prep()
  
  shinyApp(
    {fluidPage(
      includeCSS("./www/base.css"),
      fluidRow(selectpatchUI("patch")),
      theme = bslib::bs_theme(version = 3, "lumen"))
      },
           function(input, output, session){
             selectpatchServer("patch")
           })
}

app_predictiondetailsonly <- function(){
  main_app_prep()
  data <- do.call(reactiveValues, readRDS("tests/testthat/predictions_data_2patches.rds"))
  # observe({
  #   topten <- order(data$species_prob_current[, "median"], decreasing = TRUE)[1:10]
  #   data$toptennames <- row.names(data$species_prob_current)[topten]
  #   data$speciesinfo_topten <- speciesinfo[row.names(data$species_prob_current)[topten], ]
  # })
  
  shinyApp(predictionsdetailUI("detail", isolate(data$speciesinfo_topten), isolate(data$speciesinfo_botten)),
           function(input, output, session){
             predictionsdetailServer("detail", data)
           })
}