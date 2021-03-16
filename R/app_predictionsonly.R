app_predictionsonly <- function(){
  main_app_prep()
  current_values <- do.call(reactiveValues, readRDS("tests/testthat/current_values_2patches.rds"))
  
  shinyApp(predictionsUI("pred"),
           function(input, output, session){
             predictionsServer("pred", current_values,
                               model_data, new_data_mean,
                               report_path)
           })
}

app_predictiondetailsonly <- function(){
  main_app_prep()
  data <- do.call(reactiveValues, readRDS("tests/testthat/predictions_data_2patches.rds"))
  
  shinyApp(predictionsdetailUI("detail"),
           function(input, output, session){
             predictionsdetailServer("detail", data)
           })
}