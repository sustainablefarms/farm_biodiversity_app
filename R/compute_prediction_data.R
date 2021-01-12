
compute_prediction_data <- function(model_data, current_values, new_data_mean,
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
  prediction_current_wlimits = msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                            new_data)
  species_prediction_df <- data.frame(
    species = model_data$species,
    prediction_current = as.numeric(prediction_current_wlimits[, "median"]),
    prediction_mean = as.numeric(msod::poccupancy_mostfavourablesite.jsodm_lv(
      model_data,
      new_data_mean)[, "median"]),
    prediction_current_upper = as.numeric(prediction_current_wlimits[, "upper"]),
    prediction_current_lower = as.numeric(prediction_current_wlimits[, "lower"]))
  species_prediction_df$difference <- (species_prediction_df$prediction_current -
                                         species_prediction_df$prediction_mean) / species_prediction_df$prediction_mean
  
  # get dataset of top 10 most common species
  sp_current <- species_prediction_df[
    order(species_prediction_df$prediction_current, decreasing = TRUE)[1:10], c(1,2,4,5)]
  colnames(sp_current)[2] <- "value"
  colnames(sp_current)[3] <- "upper"
  colnames(sp_current)[4] <- "lower"
  
  
  # ditto for 'most different' species
  sp_different <- species_prediction_df[
    order(species_prediction_df$difference, decreasing = TRUE)[1:10], c(1, 4)]
  colnames(sp_different)[2] <- "value"
  
  species_predictions <- list(
    common = sp_current,
    different = sp_different)
  
  # richness calculations
  # get richness
  richness_data <- list(new_data, new_data, new_data)
  # richness_data[[1]]$NMdetected <- 0; richness_data[[3]]$NMdetected <- 1
  richness_data[[1]]$ms <- 0; richness_data[[3]]$ms <- 10
  richness_data[[1]]$woody500m <- 2; richness_data[[3]]$woody500m <- 20
  richness_predictions <- lapply(richness_data, function(a){
    set.seed(4444)
    msod:::specrichness_avsite.jsodm_lv(model_data, a)
  })
  richness_df <- as.data.frame(do.call(rbind, richness_predictions))
  richness_df$category <- factor(seq_len(3), levels = seq_len(3),
                                 labels = c("Less vegetation", "Your estimate", "More vegetation"))
  
  species_richness <- richness_df
  
  
  return(
    list(
      species_predictions = species_predictions,
      species_richness = species_richness
  ))
}