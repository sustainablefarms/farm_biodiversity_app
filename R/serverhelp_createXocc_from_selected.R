# creates a data frame of predictor values from the inputs.
# must have column names NMdetected, SurveyYear, WCF_500, WCF_3000, IsPlanting, plus all the climate info
# ie the same columns as model_data$XoccProcess$scale, except for logs
newXocc_fromselected <- function(current_values){
  new_data <- current_values$patchattr_tbl[, c("woody500m", "woody3000m", "IsRemnant", "noisy_miner")]
  new_data$noisy_miner <- as.numeric(new_data$noisy_miner)
  colnames(new_data)[colnames(new_data) == "noisy_miner"] <- "NMdetected"
  colnames(new_data)[colnames(new_data) == "woody500m"] <- "WCF_500"
  colnames(new_data)[colnames(new_data) == "woody3000m"] <- "WCF_3000"
  new_data$IsPlanting = as.numeric(!new_data$IsRemnant)
  new_data$IsRemnant <- NULL
  
  new_data$SurveyYear <- 2018
  # add YfA
  new_data <- cbind(new_data, current_values[grep(".YfA$", names(current_values), value = TRUE)])
  new_data$AnnPrec.YfA <- as.numeric(new_data$AnnPrec.YfA)
  # add lt
  new_data <- cbind(new_data, current_values[grep(".lt$", names(current_values), value = TRUE)])
  new_data$AnnPrec.lt <- NULL
  
  # check
  stopifnot(
    setequal(intersect(colnames(new_data), names(model_data$XoccProcess$scale)),
           grep("^[^l]", names(model_data$XoccProcess$scale), value = TRUE))
  )
  stopifnot(
    setequal(setdiff(colnames(new_data), names(model_data$XoccProcess$scale)),
      c("WCF_500", "WCF_3000"))
  )
  stopifnot(vapply(new_data, class, FUN.VALUE = "ASDA") == "numeric")
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
