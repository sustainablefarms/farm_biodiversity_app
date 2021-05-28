makeniceprops <- function(patchprops, ...){
  patchprops[row.names(patchprops) %in% c("MaxTWarmMonth.lt", "MinTColdMonth.lt"), ] <-   # convert temperatures back to celcius
    patchprops[row.names(patchprops) %in% c("MaxTWarmMonth.lt", "MinTColdMonth.lt"), ] / 10
  # patchprops[row.names(patchprops) == "NMdetected", ] <- as.logical(patchprops[row.names(patchprops) == "NMdetected", ])
  # patchprops[row.names(patchprops) == "IsPlanting", ] <- as.logical(patchprops[row.names(patchprops) == "IsPlanting", ])
  patchprops <- patchprops[c(1:5, 9, 6:8, 10:nrow(patchprops)), , drop = FALSE] #make all long-term climate at the top
  
  row.names(patchprops) <- techcovar2nicename(row.names(patchprops))
  
  patchprops_char <- format(patchprops, drop0trailing = TRUE, trim = TRUE, ...)
  patchprops_char[row.names(patchprops) == techcovar2nicename("NMdetected"), ] <- 
    as.character(as.logical(as.numeric(patchprops_char[row.names(patchprops) == techcovar2nicename("NMdetected"), ])))
  patchprops_char[row.names(patchprops) == techcovar2nicename("IsPlanting"), ] <- 
    as.character(as.logical(as.numeric(patchprops_char[row.names(patchprops) == techcovar2nicename("IsPlanting"), ])))

# Precipitation seasonality is a percentage. The R package dismo computes biovars, it imports 'cv' from package Raster (same authors). The code for 'cv' in raster has 100 * sd(z) / mean(z).
  return(patchprops_char)
}

techcovar2nicename <- function(techcovarnames){
  nicenames <- covarnicenames[techcovarnames]
  return(nicenames)
}

covarnicenames <- covarnicenames_tbl$NiceName
names(covarnicenames) <- covarnicenames_tbl$TechName
