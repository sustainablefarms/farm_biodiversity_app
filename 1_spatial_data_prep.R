# script to process spatial data for display in the farm_biodiversity_app

library(sf)
library(rmapshaper)
library(ggplot2)
library(ggtext)

# ABS SA2 regions

abs_regions <- read_sf("./data/spatial_raw/ABS_sa2_2016_aust_shape/SA2_2016_AUST.shp")

sa2_regions <- c(
  "Dubbo Region", "Dubbo - West", "Dubbo - East" , "Dubbo - South",
  "Mudgee", "Mudgee Region - West", "Mudgee Region - East",
  "Wellington",
  "Parkes (NSW)", "Parkes Region",
  "Orange Region", "Orange", "Orange - North",
  "Bathurst Region", "Bathurst", "Bathurst - East",
  "Oberon", "Forbes", "Blayney",
  "Cowra Region", "Cowra",
  "Grenfell",
  "Young", "Young Region",
  "Goulburn", "Goulburn Region",
  "Yass", "Yass Region",
  "Temora", "Cootamundra", "Junee", "Gundagai",
  "Narrandera",
  "Wagga Wagga Region", "Wagga Wagga - West", "Wagga Wagga - North",
  "Wagga Wagga - South", "Wagga Wagga - East",
  "Tumut", "Tumut Region",
  "Tocumwal - Finley - Jerilderie",
  "Corowa Region", "Corowa", "Rutherglen",
  "Albury Region", "Albury - South", "Albury - North",
  "Lavington", "Albury - East", "Tumbarumba",
  "Towong", "Wodonga", "Yackandandah", "Myrtleford",
  "Beechworth",
  "Wangaratta Region", "Wangaratta",
  "Yarrawonga", "Moira",
  "Cobram", "Numurkah", "Kyabram",
  "Shepparton Region - West", "Shepparton Region - East",
  "Shepparton - South", "Shepparton - North",
  "Mooroopna", "Nagambie", "Euroa",
  "Benalla", "Benalla Region",
  "Bright - Mount Beauty",
  "Chiltern - Indigo Valley", "West Wodonga")
abs_regions <- st_transform(
  abs_regions[abs_regions$SA2_NAME16 %in% sa2_regions, ],
  crs = "+proj=longlat +datum=WGS84")

saveRDS(abs_regions, "./app/data/spatial_sa2.rds")

# plot a single region
# centroid <- st_centroid(abs_regions[abs_regions$SA2_NAME16 == "Yass", ])
ggplot(abs_regions[abs_regions$SA2_NAME16 == "Yass", ]) +
  geom_sf(fill = NA, color = "grey30") +
  theme_void()

# plot the whole region with the highlighted region in color
ggplot(abs_regions) +
  geom_sf(fill = "grey90", color = "grey90") +
  geom_sf(data = abs_regions[abs_regions$SA2_NAME16 == "Yass", ],
    color = NA,
    fill = "#36c275") +
  ggtitle("ABS SA2 Regions", subtitle = "<p style='color:#36c275'>Region of Yass</p>") +
  theme_void() +
  theme(plot.subtitle = element_markdown())
# looks good, but a bit
