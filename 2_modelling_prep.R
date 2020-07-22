# 2_modelling_prep.R

x <- readRDS("./data/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO_extract.rds")
ms <- readRDS("./data/ms_features.rds")
woody_veg <- readRDS("./data/woody500m_features.rds")

# test making predctions based on averages
new_data <- matrix(data = x$XoccProcess$center, nrow = 1, ncol = 11)
colnames(new_data) <- names(x$XoccProcess$center)

# or using
new_data <- data.frame(
  AnnMeanTemp = 143,
  AnnPrec = 800,
  AnnTempRange = 300,
  PrecSeasonality = 18,
  SurveyYear = 2018,
  PrecWarmQ = 180,
  woody500m = 8,
  ms = 5,
  NMdetected = 1
)

# # correct input data
# u.b <- x$u.b
# u.b <- u.b[grepl("^u.b", names(u.b))]
# ub_matrix <- matrix(data = u.b, nrow = 60, ncol = 11, byrow = FALSE)
#
# # get species names
# test <- readRDS("/Users/martin_westgate/Documents/Work/Development/sustainable_farms/linking-data/private/data/clean/7_2_10_input_data.rds")
# rownames(ub_matrix) <- test$species
#
# # overwrite earlier data structure
# x$u.b <- ub_matrix
# saveRDS(x, "./app/data/model_data.rds")

# use to make predictions
model_data <- readRDS("./app/data/model_data.rds")


# get richness
# richness <- multisiterichness_nolv(new_data, x$XoccProcess, ub_matrix)
# per-species
# spp <- poccupancy_standalone_nolv(new_data, x$XoccProcess, ub_matrix)



# link this data together
species_prediction_matrix <- poccupancy_standalone_nolv(
  new_data,
  model_data$XoccProcess,
  model_data$u.b)

species_prediction_df <- data.frame(
  species = colnames(species_prediction_matrix),
  prediction_current = as.numeric(species_prediction_matrix))
species_prediction_df <- species_prediction_df[
  order(species_prediction_df$prediction_current, decreasing = TRUE), ]

species_prediction_df_small <- species_prediction_df[1:10, ]
species_prediction_df_small$species <- factor(
  seq_len(10),
  levels = seq_len(10),
  labels = species_prediction_df_small$species)


ggplot(species_prediction_df_small,
  aes(x = species, y = prediction_current, fill = prediction_current)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.05, label = species), size = 4, color = "white", hjust = 0) +
  geom_text(aes(
    y = prediction_current + 0.05,
    label = paste0(round(prediction_current*100, 0), "%"),
    color = prediction_current),
    size = 4, hjust = 0) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(species_prediction_df_small$species))) +
  expand_limits(y = c(0, 1.1)) +
  theme_void() +
  theme(legend.position = "none")