newXocc_fromselected <- function(model_data, current_values,
                    points, selected_region){
  # points <- data$points; selected_region <- data$selected_region;
  # current_values <- reactiveValuesToList(current_values)
  new_data <- data.frame(
    AnnPrec = points$AnnPrec[points$label == selected_region],
    MaxTWarmMonth = points$MaxTWarmMonth[points$label == selected_region],
    MinTColdMonth = points$MinTColdMonth[points$label == selected_region],
    PrecSeasonality = points$PrecSeasonality[points$label == selected_region],
    latitude = points$latitude[points$label == selected_region],
    SurveyYear = current_values$year,
    woody500m = current_values$woody_veg,
    ms = current_values$midstorey,
    NMdetected = as.numeric(current_values$noisy_miner)
  )
  return(new_data)
}
