devtools::load_all()
           model_data <- load_model_data()
           new_data_mean <- get_new_data_mean(model_data)
           points <- readRDS("data/sa2_points_climate.rds")
           current_values <-  readRDS("current_values_two_patches.rds")
shinyApp(predictionsUI("pred"),
         function(input, output, session){
           predictionsServer("pred", current_values,
                             model_data, new_data_mean)
           })

devtools::load_all()
model_data <- load_model_data()
new_data_mean <- get_new_data_mean(model_data)
current_values <-  readRDS("current_values_two_patches.rds")
current_values$AnnPrec <- NULL
shinyApp(predictionsUI("pred"),
         function(input, output, session){
           predictionsServer("pred", current_values,
                             model_data, new_data_mean)
         })
