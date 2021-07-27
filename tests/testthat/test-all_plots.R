# test that all plots can be built
devtools::load_all()
library(msod)

test_that("prediction plots for Gundagai and a single patch", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  data <- list()
  current_values <- readRDS(system.file("data", "test-current_values_2patches.rds", package = packageName()))
  current_values <- isolate(current_values)
    data$Xocc <- newXocc_fromselected(current_values)
    modwXocc <- msod::supplant_new_data(model_data, data$Xocc, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                         model_data$XoccProcess$scale,
                                                                         model_data$XoccColNames)})
    modwmeanXocc <- msod::supplant_new_data(model_data, new_data_mean, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                         model_data$XoccProcess$scale,
                                                                         model_data$XoccColNames)})
    data$species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(modwXocc)
    data$species_prob_ref <- msod::poccupancy_mostfavourablesite.jsodm_lv(modwmeanXocc)
    data$spec_different <- todifferent(data$species_prob_current, data$species_prob_ref)
    data$species_richness <- compute_richness(model_data, data$Xocc)
    topten <- order(data$species_prob_current[, "median"], decreasing = TRUE)[1:10]
    botten <- order(data$species_prob_current[, "median"], decreasing = FALSE)[1:10]
    data$toptennames <- row.names(data$species_prob_current)[topten]
    data$speciesinfo_topten <- speciesinfo[row.names(data$species_prob_current)[topten], ]
    data$speciesinfo_botten <- speciesinfo[row.names(data$species_prob_current)[botten], ]
  
    richness_plot(data$species_richness)
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
