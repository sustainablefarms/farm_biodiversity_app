# 2_modelling_prep.R

# x <- readRDS("./data/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO_extract.rds")
# ms <- readRDS("./data/ms_features.rds")
# woody_veg <- readRDS("./data/woody500m_features.rds")

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

# test making predctions based on averages
new_data_mean <- as.data.frame(matrix(data = model_data$XoccProcess$center, nrow = 1, ncol = 11))
colnames(new_data_mean) <- names(model_data$XoccProcess$center)
new_data_mean <- new_data_mean[, c(2:10)]
new_data_mean$NMdetected[1] <- 1

# or using user inputs
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


# link this data together
# species_prediction_matrix <- poccupancy_standalone_nolv(
#   new_data,
#   model_data$XoccProcess,
#   model_data$u.b)

species_prediction_df <- data.frame(
  species = rownames(model_data$u.b),
  prediction_current = as.numeric(poccupancy_standalone_nolv(
    new_data,
    model_data$XoccProcess,
    model_data$u.b)),
  prediction_mean = as.numeric(poccupancy_standalone_nolv(
    new_data_mean,
    model_data$XoccProcess,
    model_data$u.b))
  )
species_prediction_df$difference <- species_prediction_df$prediction_current -
  species_prediction_df$prediction_mean

# get dataset of top 10 most common species
sp_current <- species_prediction_df[
  order(species_prediction_df$prediction_current, decreasing = TRUE)[1:10], c(1:2)]
colnames(sp_current)[2] <- "value"

# ditto for 'most different' species
sp_different <- species_prediction_df[
  order(species_prediction_df$difference, decreasing = TRUE)[1:10], c(1, 4)]
colnames(sp_different)[2] <- "value"

species_ggplot(sp_current)
species_ggplot(sp_different, add_plus = TRUE)

# get richness
richness_data <- list(new_data, new_data, new_data)
richness_data[[1]]$ms <- 0; richness_data[[3]]$ms <- 10

richness_data[[1]] <- richness_data[[1]][rep(1, 3), ]

richness_data[[1]]$woody500m <- 2; richness_data[[3]]$woody500m <- 20
richness_data[[3]] <- do.call(rbind, richness_data[[3]][rep(1, nrow(new_data)), ])
richness_predictions <- lapply(richness_data, function(a){
  multisiterichness_nolv(a, model_data$XoccProcess, model_data$u.b)
})
richness_df <- as.data.frame(do.call(rbind, richness_predictions))
richness_df$category <- factor(seq_len(3), levels = seq_len(3),
  labels = c("Less vegetation", "Your estimate", "More vegetation"))

ggplot(richness_df, aes(x = category, y = Erichness, fill = category)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y = c(0, max(richness_df$Erichness + richness_df$Vrichness) + 3)) +
  scale_x_discrete(position = "top") +
  scale_discrete_manual(aesthetics = "fill", values = c("#81a2b3", "#4e839c", "#81a2b3")) +
  geom_errorbar(aes(ymin = Erichness - Vrichness, ymax = Erichness + Vrichness), width = 0.2) +
  coord_flip() +
  ggtitle("Number of bird species") +
  theme(legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.ticks.y = element_blank(),
    panel.grid.minor.x = element_line(color = "grey80"),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = NA),
    panel.border = element_blank()
  )


