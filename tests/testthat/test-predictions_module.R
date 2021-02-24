devtools::load_all()
model_data <- load_model_data()
traits <- read.csv("../sflddata/private/data/raw/Australian_Bird_Data_Version_1.csv", 
                   stringsAsFactors = FALSE) %>%
  dplyr::filter(X3_Taxon_common_name_2 %in% model_data$species) %>%
  dplyr::select(`Common Name` = X3_Taxon_common_name_2,
                `Scientific Name` = X7_Taxon_scientific_name_CandB_2, 
                `Body Length` = X96_Body_length_8,
                `Body Mass` = X99_Body_mass_average_8) %>%
  dplyr::mutate(`Body Length` = as.numeric(`Body Length`),
                `Body Mass` = as.numeric(`Body Mass`)) 
new_data_mean <- get_new_data_mean(model_data)
points <- readRDS("data/sa2_points_climate.rds")
current_values <-  readRDS("current_values_two_patches.rds")

tempdir <- tempdir()
report_path <- paste0(tempdir, "/", "report.Rmd") #file location assumes host is a unix machine
stopifnot(file.copy("report.Rmd", report_path, overwrite = TRUE)) 


shinyApp(predictionsUI("pred"),
         function(input, output, session){
           predictionsServer("pred", current_values,
                             model_data, new_data_mean,
                             report_path)
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
