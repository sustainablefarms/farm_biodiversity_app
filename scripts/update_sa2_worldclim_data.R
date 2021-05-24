library(dplyr)
sa2_points <- readRDS("./data/sa2_points.rds")
sa2_points <- sa2_points[, c("longitude", "latitude", "label", "state", "color")]

# remove young and tumut region
sa2_points <- sa2_points[!(sa2_points$label %in% c("Young", "Tumut Region")), ]

# Get worldclim data at locations
# NOTE: This script will drop a file called wc0.5 in your home directory
  # you can delete it when done

locs <- sf::st_as_sf(sa2_points, coords = c("longitude", "latitude"), crs = 4326)
result_df <- sflddata::ll2worldclim(locs)


# cut down to only data that the model needs
result_df <- result_df[, c(
"AnnMeanTemp",
"AnnPrec",
"MaxTWarmMonth",
"PrecWarmQ",
"MinTColdMonth",  
"PrecColdQ",
"PrecSeasonality")]
result_df <- cbind(sa2_points, result_df)

# check edges of climate data
climnames <- readRDS(system.file("climate_names_table.rds", package = "sflddata"))
indata <- readRDS(paste0(readLines("../Experiments/link_sflddata.txt"), "clean/allsites_data_experiment10_cleaned.rds"))
quantiles <- indata$insample$Xocc %>%
  dplyr::distinct(StudyCode, SurveySiteId, SiteCode, .keep_all = TRUE) %>%
  dplyr::select(any_of(paste0(colnames(result_df), ".lt")), any_of(paste0(colnames(result_df), ".YfA"))) %>%
  # summarise(AMT.lt = quantile(AnnMeanTemp.lt, probs = c(0, 0.05, 0.95, 1)))
  summarise(across(where(is.numeric), ~quantile(.x, probs = c(0, 0.05, 0.95, 1))))

#remove regions whose climate variables are outside the training data, except for regions that are only just outside
result_df_cut <- result_df %>%
  dplyr::filter((quantiles$MaxTWarmMonth.lt[[1]]  <= MaxTWarmMonth) & (MaxTWarmMonth <= quantiles$MaxTWarmMonth.lt[[4]])) %>%
  dplyr::filter((quantiles$PrecWarmQ.lt[[1]]-2  <= PrecWarmQ) & (PrecWarmQ <= quantiles$PrecWarmQ.lt[[4]] + 2)) %>%
  dplyr::filter((quantiles$MinTColdMonth.lt[[1]]  <= MinTColdMonth) & (MinTColdMonth <= quantiles$MinTColdMonth.lt[[4]])) %>%
  dplyr::filter((quantiles$PrecColdQ.lt[[1]]  <= PrecColdQ) & (PrecColdQ <= quantiles$PrecColdQ.lt[[4]] + 10)) %>%
  dplyr::filter((quantiles$PrecSeasonality.lt[[1]]  <= PrecSeasonality) & (PrecSeasonality <= quantiles$PrecSeasonality.lt[[4]])) %>%
  dplyr::filter((quantiles$AnnPrec.YfA[[2]]  <= AnnPrec) & (AnnPrec <= quantiles$AnnPrec.YfA[[3]]))
  # ggplot() +
  # geom_point(aes(x= longitude, y = latitude, color = color),
             # show.legend = FALSE)




saveRDS(result_df_cut, "./data/sa2_points_climate.rds")
