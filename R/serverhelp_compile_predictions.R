# current_values <- readRDS("./current_values.rds")
# refpredictions <- species_prob_mean
compile_predictions <- function(current_values, refpredictions, refisaverage = TRUE){
  data <- list()
  data$Xocc <- newXocc_fromselected(current_values)
  modwXocc <- msod::supplant_new_data(model_data, data$Xocc, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                       model_data$XoccProcess$scale,
                                                                       model_data$XoccColNames)})
  print(modwXocc$data$Xocc)
  data$spec_prob <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwXocc)
  species_richness_raw <- rbind(compute_richness(model_data, data$Xocc),
                                 reference = sum(refpredictions[, "median"])) 
  category_name <- c(
    "high" = sprintf("%sNearby woody cover = 20%%", if(refisaverage){""}else{"S.2: "}),
    "low" = sprintf("%sNearby woody cover = 2%%", if(refisaverage){""}else{"S.2: "}),
    "reference" = if (refisaverage){"Average"} else {"Scenario 1"},
    "current" = if (refisaverage){"Scenario 1"} else {"Scenario 2"})
  category_name_f <- factor(category_name, levels = category_name, ordered = TRUE)
  species_richness_raw$category <- category_name_f[rownames(species_richness_raw)]
  data$species_richness <- species_richness_raw
  data <- c(data, predictions_morecontext(data$spec_prob, refpredictions, refisaverage))
  return(data)
}

predictions_morecontext <- function(spec_prob, refpredictions, refisaverage){
  data <- list()
  data$spec_different <- todifferent(spec_prob, refpredictions)
  topten <- order(spec_prob[, "median"], decreasing = TRUE)[1:10]
  botten <- order(spec_prob[, "median"], decreasing = FALSE)[1:10]
  data$toptennames <- row.names(spec_prob)[topten]
  data$speciesinfo_topten <- speciesinfo[row.names(spec_prob)[topten], ]
  data$speciesinfo_botten <- speciesinfo[row.names(spec_prob)[botten], ]
  return(data)
}
