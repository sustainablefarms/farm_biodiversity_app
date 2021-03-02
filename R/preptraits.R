# prep and load traits

preptraits <- function(model_data){
  traits <- read.csv("../sflddata/private/data/raw/Australian_Bird_Data_Version_1.csv", 
           stringsAsFactors = FALSE) %>%
    dplyr::filter(X3_Taxon_common_name_2 %in% model_data$species) %>%
    dplyr::select(`Common Name` = X3_Taxon_common_name_2,
                  `Scientific Name` = X7_Taxon_scientific_name_CandB_2, 
                  `Body Length` = X96_Body_length_8,
                  `Body Mass` = X99_Body_mass_average_8) %>%
    dplyr::mutate(`Body Length` = as.numeric(`Body Length`),
                  `Body Mass` = as.numeric(`Body Mass`))
  saveRDS(traits, file = "./data/traits.rds")
  return(traits)
}

loadtraits2global <- function(){
  traits <<- readRDS("./data/traits.rds")
  return(traits)
}
