parsechar <- function(charstring, label){ #label used for the error message
  out <- suppressWarnings(as.numeric(charstring))
  if (is.na(out)){stop(simpleError(paste(label, "could not be interpreted as numerical.")))}
  return(out)
}

checkfinalwcfs <- function(wcfs){
  if (!is.null(wcfs)){
    if (any(wcfs < 2) | any(wcfs > 20)){
      warnstart <-sprintf("Warning: Woody vegetation cover at the selected location is outside the capabilities of the %s model.", appname)
      warnend <- paste("Treat results from", appname, "with an extra degree of caution.")
      # wcfs[[1]] < 2, wcfs[[2]] < 2, both (and none > 20)
      # wcfs[[1]] > 20, wcfs[[2]] > 20, both
      # others would be much rarer, print a more generic
      specwarn <- NULL
      if (any(wcfs < 2) & all(wcfs <= 20)){
         if (all(wcfs < 2)){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m and %3.1f%% of the area within 3km. This is too low for accurate estimates.", wcfs[[1]], wcfs[[2]])
	 } else if (wcfs[[1]] < 2){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m, which is too low for accurate estimates.", wcfs[[1]])
	 } else if (wcfs[[2]] < 2){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 3km, which is too low for accurate estimates.", wcfs[[2]])
	 }
      }
      if (any(wcfs > 20) & all(wcfs >= 2)){
         if (all(wcfs > 20)){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m and %3.1f%% of the area within 3km. This is too high for accurate estimates.", wcfs[[1]], wcfs[[2]])
	 } else if (wcfs[[1]] > 20){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m, which is too high for accurate estimates.", wcfs[[1]])
	 } else if (wcfs[[2]] > 20){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 3km, which is too high for accurate estimates.", wcfs[[2]])
	 }
      }
      if (is.null(specwarn)){
      specwarn <- sprintf("Woody vegetation canopy covered %3.1f%% of the area within 500m and %3.1f%% of the area within 3km.", wcfs[[1]], wcfs[[2]])
      }
      warning(simpleWarning(
	paste(warnstart, specwarn, warnend)
      ))
    }
  }
  invisible(wcfs)
} 

canopyfromlatlon <- function(lon, lat, year){
  if (any(is.na(lat), is.na(lon), is.na(year))){return(NULL)}
  point <- sf::st_point(x = c(lon, lat), dim = "XY")
  pointwcrs <- sf::st_sf(sf::st_sfc(point, crs = 4326))

  if (file.exists("./data/wcfserver.txt")) {
    cat("Using wcfserver\n")
    within500m <- cloudget(pointwcrs, 500) %>% extractayear(year)
    within3000m <- cloudget(pointwcrs, 3000) %>% extractayear(year)
    out <- c(within500m, within3000m)
  } else {
    within500m <- threddsget(pointwcrs, 500, year)
    within3000m <- threddsget(pointwcrs, 3000, year)
    out <- data.frame(within500m, within3000m)
  }

  names(out) <- c("500m", "3000m")
  if (any(is.na(out))){stop(simpleError("Data is missing for this location."))}
  return(out)
}

cloudget <- function(pointwcrs, bufferdist){
  # compute the buffer polygon
  pointAA <- sf::st_transform(pointwcrs, 3577) #to GDA94 / Aust Albers so that buffers in metres make sense
  if (isTRUE(all.equal(sf::st_bbox(pointAA), sf::NA_bbox_, check.attributes = FALSE))){stop("Invalid coordinates.")}
  buf <- sf::st_transform(sf::st_buffer(pointAA, dist = bufferdist), crs = 4326)
  jsontxt <- geojsonsf::sf_geojson(buf, simplify = FALSE)
  
  # modify json to have info compatible to the server (this info is guessed from a request from Pablo)
  jsonobj <- jsonlite::parse_json(jsontxt)
  jsonobj <- jsonobj[["features"]][1]
  names(jsonobj) <- "feature"
  jsonobj <- c(jsonobj, 
    list(
    in_crs=  "epsg:4326",
    out_crs = "epsg:3577",
    resolution= -1,
    expr= "mean:space(WCF.wcf)",
    output= "csv"
    )
  )
  jsontxt <- jsonlite::toJSON(jsonobj, auto_unbox = TRUE)
  
  # send request to server
  returned <- httr::POST(
    url = readLines("./data/wcfserver.txt"),
    body = jsontxt
  )
  returned <- httr::stop_for_status(returned, task = "load woody cover")
  values_allyears <- httr::content(returned, type = "text/csv", encoding = "UTF-8",
    col_types = "Dd") # the server sends back all years
  colnames(values_allyears)[[2]] <- "WCF"
  values_allyears$Year <- as.integer(format(values_allyears$time, "%Y"))
  return(values_allyears[, c("Year", "WCF")])
}

threddsget <- function(pointwcrs, bufferdist, years){ # errors currently - produce NA values when should be good values!
  # compute the buffer polygon
  pointAA <- sf::st_transform(pointwcrs, 3577) #to GDA94 / Aust Albers so that buffers in metres make sense
  buf <- sf::st_buffer(pointAA, dist = (bufferdist + 50) * 1.3)
  
  wcf <- woody_vals_buffer(buf, pointAA, years, bufferdist)
  return(wcf)
}

extractayear <- function(obj, year){
  if (!(year %in% obj$Year)){stop(simpleError("Year not available."))}
  obj[obj$Year == year, "WCF"]
}
