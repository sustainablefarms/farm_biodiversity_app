minimisequerystring <- function(querystring){
  root <- gsub("_inputs_.*", "", querystring)
  valuesonlystring <- gsub(".*_values_","_values_", querystring)
  # if lp (landing page) is true then delete most of the string - landing page only seen at start
  lpcharloc <- regexpr("&lp=[^&]*", valuesonlystring)
  lpstr <- substr(valuesonlystring, lpcharloc, lpcharloc + attr(lpcharloc, "match.length") - 1)
  lpstr <- gsub("^&lp=", "", lpstr)
  lpval <- as.logical(lpstr)
  if (isTruthy(lpval)){return(root)}
  valuesprsd <- parseQueryString(valuesonlystring)
  maintabloc <- regexpr("maintabs=[^&]*", querystring)
  maintabsstring <- substr(querystring, maintabloc, maintabloc + attr(maintabloc, "match.length") - 1)
  
  combinedstring <- paste("_inputs_", maintabsstring, valuesonlystring, sep = "&")
  return(paste0(root, combinedstring))
}

abbrpatchtblnames <- c(
  "woody500m" = "wn",
  "woody3000m" = "wr",
  "pid" = "p",
  "IsRemnant" = "r",
  "noisy_miner" = "n"
)
abbrpatchtblnames2 <- names(abbrpatchtblnames)
names(abbrpatchtblnames2) <- abbrpatchtblnames
compactattrtable <- function(tbl){
  if (!isTruthy(tbl)){return(NULL)}
  tbl <- tbl[intersect(names(abbrpatchtblnames), names(tbl))]
  if ("IsRemnant" %in% names(tbl)){tbl$IsRemnant <- as.integer(tbl$IsRemnant)}
  if ("noisy_miner" %in% names(tbl)){tbl$noisy_miner <- as.integer(tbl$noisy_miner)}
  
  names(tbl) <- abbrpatchtblnames[names(tbl)]
  return(tbl)
}

urltable2attrtbl <- function(tbl){
  if (!isTruthy(tbl)){return(NULL)}
  tbl <- as.data.frame(tbl)
  names(tbl) <- abbrpatchtblnames2[names(tbl)]
  if ("IsRemnant" %in% names(tbl)){tbl$IsRemnant <- as.logical(tbl$IsRemnant)}
  if ("noisy_miner" %in% names(tbl)){tbl$noisy_miner <- as.logical(tbl$noisy_miner)}
  return(tbl)
}
