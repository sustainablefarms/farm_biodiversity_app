newXocc_fromselected <- function(current_values){
  # points <- data$points; selected_region <- data$selected_region;
  # current_values <- reactiveValuesToList(current_values)
  new_data <- data.frame(
    AnnPrec = current_values$AnnPrec,
    MaxTWarmMonth = current_values$MaxTWarmMonth,
    MinTColdMonth = current_values$MinTColdMonth,
    PrecSeasonality = current_values$PrecSeasonality,
    latitude = current_values$latitude,
    SurveyYear = current_values$year,
    woody500m = current_values$woody_veg,
    ms = current_values$midstorey,
    NMdetected = as.numeric(current_values$noisy_miner)
  )
  return(new_data)
}
