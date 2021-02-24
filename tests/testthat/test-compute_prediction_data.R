test_that("compute prediction data runs on Gundagai", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  current_values <- isolate(reactiveValuesToList(readRDS("./current_values_one_patch.rds")))
  Xocc <- newXocc_fromselected(current_values)
  species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                       Xocc)
  species_prob_ref <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                   new_data_mean)
  spec_different <- todifferent(species_prob_current, species_prob_ref)
  species_richness <- compute_richness(model_data, Xocc)

})
