# test that all plots can be built
devtools::load_all()
library(msod)

test_that("prediction plots for Gundagai and a single patch", {
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
  
  richness_plot(species_richness)
  species_ggplot(
    df = tocommon(species_prob_current),
    title = "Most likely species",
    add_plus = FALSE,
    errorbar = TRUE)
  species_ggplot(
    df = spec_different,
    title = "Locally prevalent species",
    add_plus = TRUE)
  species_plotly(
    df = tocommon(species_prob_current),
    title = "Most likely species at any patch",
    add_plus = FALSE,
    errorbar = TRUE)
})

test_that("prediction plots for Gundagai and two patches", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  current_values <- isolate(reactiveValuesToList(readRDS("./current_values_two_patches.rds")))
  Xocc <- newXocc_fromselected(current_values)
  species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                       Xocc)
  species_prob_ref <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                   new_data_mean)
  spec_different <- todifferent(species_prob_current, species_prob_ref)
  species_richness <- compute_richness(model_data, Xocc)
  
  richness_plot(species_richness)
  species_ggplot(
    df = tocommon(species_prob_current),
    title = "Most likely species",
    add_plus = FALSE,
    errorbar = TRUE)
  species_ggplot(
    df = spec_different,
    title = "Locally prevalent species",
    add_plus = TRUE)
  species_plotly(
    df = tocommon(species_prob_current),
    title = "Most likely species at any patch",
    add_plus = FALSE,
    errorbar = TRUE)
})

test_that("modalplots", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  selected_region <- "Gundagai"
  current_values <- isolate(reactiveValuesToList(readRDS("./current_values_one_patch.rds")))
  species_ggplotInModal(model_data, current_values, new_data_mean)
})
