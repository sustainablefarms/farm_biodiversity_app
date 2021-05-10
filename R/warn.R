# out-of-training data warn
warn_oot <- function(cvals){
  warn <- FALSE
  reason <- ""
  # Western Murray: high regional woody veg estimates high species richness at v low , and high nearby woody vegetation
  if (any(grepl("(Corowa|Narrandera|Tocumwal)", cvals["selected_region"], ignore.case = TRUE)) &&
      cvals[["woody3000m"]] > 10){
    warn <- TRUE
    reason <- paste(reason,
                     "In your geographich region our training data contained only one site",
                     "with greater than 10% woody vegetation canopy within 3km.",
                     "Bird estimates here should be treated with additional caution."
                     )
  }
  return(list(
    warn = warn,
    reason = reason
  ))
}