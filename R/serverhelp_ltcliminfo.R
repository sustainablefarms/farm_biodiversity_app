ltcliminfo_region <- function(selected_region, climdatatbl){
  locinfo <- list()
  locinfo$selected_region <- selected_region
  # add climate data
  climate_row <- which(climdatatbl$label == locinfo$selected_region)
  locinfo$MaxTWarmMonth.lt <- climdatatbl$MaxTWarmMonth[climate_row]
  locinfo$PrecWarmQ.lt <- climdatatbl$PrecWarmQ[climate_row]
  locinfo$MinTColdMonth.lt <- climdatatbl$MinTColdMonth[climate_row]
  locinfo$PrecColdQ.lt <- climdatatbl$PrecColdQ[climate_row]
  locinfo$PrecSeasonality.lt <- climdatatbl$PrecSeasonality[climate_row]
  
  locinfo$AnnPrec.lt <- climdatatbl$AnnPrec[climate_row]
  locinfo$AnnMeanTemp.YfA <- climdatatbl$AnnMeanTemp[climate_row]/10
  locinfo$MaxTWarmMonth.YfA <- new_data_mean$MaxTWarmMonth.YfA
  locinfo$PrecWarmQ.YfA <- new_data_mean$PrecWarmQ.YfA
  locinfo$MinTColdMonth.YfA <- new_data_mean$MinTColdMonth.YfA
  locinfo$PrecColdQ.YfA <- new_data_mean$PrecColdQ.YfA
  locinfo$PrecSeasonality.YfA <- new_data_mean$PrecSeasonality.YfA
  if (isTruthy(locinfo$selected_region)){
    locinfo$locationcomplete <- TRUE
  } else {
    locinfo$locationcomplete <- FALSE
  }
  return(locinfo)
}