app_predictionsonly <- function(){
  # Data Preparations
  model_data <<- load_model_data()
  new_data_mean <<- get_new_data_mean(model_data)
  
  # preptraits(model_data)
  loadtraits2global()
  tempdir <- tempdir()
  report_path <<- paste0(tempdir, "/", "report.Rmd") #file location assumes host is a unix machine
  stopifnot(file.copy("report.Rmd", report_path, overwrite = TRUE)) 
  
  model_data <- load_model_data()
  current_values <- do.call(reactiveValues, readRDS("tests/testthat/current_values_2patches.rds"))
  
  shinyApp(predictionsUI("pred"),
           function(input, output, session){
             predictionsServer("pred", current_values,
                               model_data, new_data_mean,
                               report_path)
           })
}