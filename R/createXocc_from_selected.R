newXocc_fromselected <- function(current_values){
  # points <- data$points; selected_region <- data$selected_region;
  # current_values <- reactiveValuesToList(current_values)
  new_data <- as.data.frame(
    current_values[!(names(current_values) %in% c("selected_region", "locationcomplete", "allpatchcomplete", "patches"))]
  )
  new_data$noisy_miner <- as.numeric(new_data$noisy_miner)
  colnames(new_data)[colnames(new_data) == "noisy_miner"] <- "NMdetected"
  colnames(new_data)[colnames(new_data) == "year"] <- "SurveyYear"
  colnames(new_data)[colnames(new_data) == "woody500m"] <- "WCF_500"
  colnames(new_data)[colnames(new_data) == "woody3000m"] <- "WCF_3000"
  new_data$IsPlanting = as.numeric(!new_data$IsRemnant)
  new_data$IsRemnant <- NULL
  return(new_data)
}

# scale and shift data
stdXocc <- function(Xocc, center, scale, XoccColNames){
  Xocc$log.WCF_500. <- log(Xocc$WCF_500)
  Xocc$log.WCF_3000. <- log(Xocc$WCF_3000)
  out <- scale(Xocc[, names(center), drop = FALSE],
        center = center,
        scale = scale)
  out <- cbind(`(Intercept)` = 1, out)
  return(out[, XoccColNames, drop = FALSE])
}
