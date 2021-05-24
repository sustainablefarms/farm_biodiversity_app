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
  new_data_mean$WCF_500 <-  9.48 #average percent - baked in as average log value is not log of average
  new_data_mean$WCF_3000 <- 9.07
  new_data_mean$log.WCF_500. <- NULL
  new_data_mean$log.WCF_3000. <- NULL
  return(new_data_mean)
}

### Code used to build saved fitted object ###
# fit <- readRDS("../Experiments/10_withAlbertData/fittedmodels/10_7_0_best1065_timewind_2lv.rds")
# fit2 <- msod::minimise_fit.jsodm_lv(fit)
# fit2$XoccProcess <- fit$toXocc$params$mainparams
# fit2$XoccColNames <- colnames(fit$data$Xocc)
# saveRDS(fit2, file = "data/model_data.rds")
