# script to process spatial data for display in the farm_biodiversity_app

library(sf)
library(rmapshaper)
library(ggplot2)
library(ggtext)
library(plotly)

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

abs_regions <- abs_regions[abs_regions$SA2_NAME16 %in% sa2_regions, ]


# transform to allow calculation of centroids and areas
abs_transformed <- st_transform(abs_regions, 3577) #st_crs = "+proj=longlat +datum=GDA94")
centroids <- st_centroid(abs_transformed)
centroids$area <- as.numeric(st_area(abs_regions))
# st_is_longlat(abs_regions) # should be FALSE

# ggplot(centroids) +
#   geom_sf(mapping = aes(color = log10(area)))

# convert back to long/lat
centroids_longlat <- st_transform(centroids, "+proj=longlat +datum=AGD84")

# worth excluding sites < particular size? These are urban so probably better ignored

ggplot(abs_regions[abs_regions$SA2_NAME16 == "Yass", ]) +
  geom_sf(fill = NA, color = "grey30") +
  theme_void()

# plot the whole region with the highlighted region in color
ggplot(abs_regions) +
  geom_sf(fill = "grey90", color = "grey30") +
  geom_sf(data = abs_regions[abs_regions$SA2_NAME16 == "Yass", ],
    color = NA,
    fill = "#36c275") +
  geom_sf(
    data = centroids_longlat,
    mapping = aes(color = cut(log10(centroids_longlat$area), breaks = c(0, 8.5, 20)))
  ) +
  ggtitle("ABS SA2 Regions", subtitle = "<p style='color:#36c275'>Region of Yass</p>") +
  theme_void() +
  theme(
    plot.subtitle = element_markdown(),
    legend.position = "none"
  )
# looks like the breaks are correct on this one


kept_centroids <- centroids_longlat[log10(centroids_longlat$area) > 8.5, ]
plot_data <- as.data.frame(do.call(rbind, lapply(
  strsplit(as.character(kept_centroids$geometry), ", "),
  function(a){as.numeric(gsub("c\\(|\\)", "", a))})))
colnames(plot_data) <- c("longitude", "latitude")
plot_data$label <- kept_centroids$SA2_NAME16
plot_data$state <- kept_centroids$STE_NAME16
plot_data$color <- c("#4e9c63", "#4e839c")[as.numeric(as.factor(plot_data$state))]
saveRDS(plot_data, "./app/data/sa2_points.rds")
# ggplotly(ggplot(plot_data, aes(x = longitude, y = latitude, color = state)) +
#   geom_point() +
#   theme_void())


saveRDS(abs_regions[abs_regions$SA2_NAME16 %in% plot_data$label, ], "./app/data/sa2_polygons.rds")

# example plot
plot_ly(
  plot_data,
  x = ~longitude,
  y = ~latitude,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 10,
    color = ~color
  ),
  hoverinfo = "text",
  text = ~label
) %>% layout(
  xaxis = list(nticks = 5),
  yaxis = list(scaleanchor = "x")
)


) %>%
  add_markers(
    x = ~longitude,
    y = ~latitude,
    color = ~state)

  colors = c("red", "blue")
)