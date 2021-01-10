# load and process model data
load_model_data <- function(modelfile = "data/model_data.rds"){
  model_data <- readRDS(modelfile)
  return(model_data)
}

get_new_data_mean <- function(model_data){
  new_data_mean <- as.data.frame(
    matrix(data = model_data$XoccProcess$center, nrow = 1))
  colnames(new_data_mean) <- names(model_data$XoccProcess$center)
  # new_data_mean <- new_data_mean[, c(2:10)]
  new_data_mean$NMdetected[1] <- 1
  new_data_mean$woody500m <- exp(new_data_mean$log.woody500m.)
  new_data_mean$log.woody500m. <- NULL
  return(new_data_mean)
}
