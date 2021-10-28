current_values <- readRDS("./current_values.rds")
refpredictions <- species_prob_mean
compile_predictions <- function(current_values, refpredictions){
  data <- list()
  data$Xocc <- newXocc_fromselected(current_values)
  modwXocc <- msod::supplant_new_data(model_data, data$Xocc, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                       model_data$XoccProcess$scale,
                                                                       model_data$XoccColNames)})
  print(modwXocc$data$Xocc)
  data$species_prob_current <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwXocc)
  data$spec_different <- todifferent(data$species_prob_current, refpredictions)
  species_richness_raw <- rbind(compute_richness(model_data, data$Xocc),
                                 reference = sum(refpredictions[, "median"]))           # add in reference
  species_richness_raw$category <- factor(1:4, levels = 4:1,
         labels = c(
                    "Reference estimate",
                    "Nearby woody cover = 20%",
                    "Your estimate",
           "Nearby woody cover = 2%"
           ),
         ordered = TRUE
  )
  data$species_richness <- species_richness_raw
  
  topten <- order(data$species_prob_current[, "median"], decreasing = TRUE)[1:10]
  botten <- order(data$species_prob_current[, "median"], decreasing = FALSE)[1:10]
  data$toptennames <- row.names(data$species_prob_current)[topten]
  data$speciesinfo_topten <- speciesinfo[row.names(data$species_prob_current)[topten], ]
  data$speciesinfo_botten <- speciesinfo[row.names(data$species_prob_current)[botten], ]
  return(data)
}