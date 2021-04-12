devtools::load_all()

test_that("prediction plots for Gundagai and a single patch using plotly", {
  model_data <- load_model_data()
  new_data_mean <- get_new_data_mean(model_data)
  current_values <- readRDS("./tests/testthat/current_values_2patches.rds")
  Xocc <- newXocc_fromselected(current_values)
  species_prob_current <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                       Xocc)
  species_prob_ref <- msod::poccupancy_mostfavourablesite.jsodm_lv(model_data,
                                                                   new_data_mean)
  spec_different <- todifferent(species_prob_current, species_prob_ref)

  species_plotly_common(tocommon(species_prob_current))
  species_plotly_different(spec_different)
  species_plotly_both(species_prob_current, spec_different)
})