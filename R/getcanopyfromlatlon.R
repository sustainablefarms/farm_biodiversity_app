parsechar <- function(charstring, label){ #label used for the error message
  out <- suppressWarnings(as.numeric(charstring))
  if (is.na(out)){stop(simpleError(paste(label, "could not be interpreted as numerical.")))}
  return(out)
}

checkfinalwcfs <- function(wcfs){
  if (!is.null(wcfs)){
    if (any(wcfs < 2) | any(wcfs > 20)){
      warning(simpleWarning(
        paste(sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m and %3.1f%% of the area within 3km.", wcfs[[1]], wcfs[[2]]),
              "At least one of these percentages is outside the capabilities of the model.",
              "Treat any estimates of bird occupancy with an extra degree of caution.")
      ))
    }
  }
  invisible(wcfs)
} 

canopyfromlatlon <- function(lon, lat, year){
  if (any(is.na(lat), is.na(lon), is.na(year))){return(NULL)}
  point <- sf::st_point(x = c(lon, lat), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))
 
  within500m <- cloudget(pointwcrs, 500, year)
  within3000m <- cloudget(pointwcrs, 3000, year)
  # threddsget(pointwcrs, 500, 2018:2019) #errors currently!

  out <- c(within500m, within3000m) 
  names(out) <- c("500m", "3000m")
  print(out)
  if (any(out == -9999)){stop(simpleError("Data is missing for this location."))}
  return(out)
}

cloudget <- function(pointwcrs, bufferdist, year){
  # compute the buffer polygon
  pointAA <- sf::st_transform(pointwcrs, 3577) #to GDA94 / Aust Albers so that buffers in metres make sense
  buf <- sf::st_transform(sf::st_buffer(pointAA, dist = bufferdist), crs = 4326)
  json <- geojsonsf::sf_geojson(buf, simplify = FALSE)
  
  # modify json to have info compatible to the server (this info is guessed from a request from Pablo)
  prefix <- "{\"layer_name\":\"wcf\",\"vector\":"
  json <- gsub("{\"type\":\"FeatureCollection\",\"features\":[", prefix, as.character(json), fixed = TRUE)
  json <- gsub("\\]\\}$", "}", json)
  
  # send request to server
  returned <- httr::POST(
    url = "https://australia-southeast1-wald-1526877012527.cloudfunctions.net/tree-change-drill",
    body = json
  )
  values_allyears <- httr::content(returned, type = "text/csv", encoding = "UTF-8",
                             col_names = c("Year", "WCF"), col_types = "id") # the server sends back all years
  if (!(year %in% values_allyears$Year)){stop(simpleError("Year not available."))}
  return(extractayear(values_allyears, year))
}

threddsget <- function(pointwcrs, bufferdist, years){ # errors currently - produce NA values when should be good values!
  # compute the buffer polygon
  pointAA <- sf::st_transform(pointwcrs, 3577) #to GDA94 / Aust Albers so that buffers in metres make sense
  buf <- sf::st_buffer(pointAA, dist = (bufferdist + 50) * 1.3)
  
  wcf <- sflddata::woody_vals_buffer(buf, pointAA, years, bufferdist)
  return(wcf)
}

extractayear <- function(obj, year){
  obj[obj$Year == year, "WCF"]
}
