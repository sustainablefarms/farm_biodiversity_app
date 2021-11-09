
# jagged <- list(list(woody500m = 9.5, woody3000m = 9, usedlon = NULL, pid = 1))
# jagged <- list(list(woody500m = 9.5, woody3000m = 9, usedlon = NULL, pid = 1),
#                list(NULL))

jagged_2_df <- function(attr_out_list){
  outtable <- dplyr::bind_rows(attr_out_list)
  # outtable <- data.frame(
  #   pid = vapply(attr_out_list, function(x) x[["pid"]], FUN.VALUE = 3),
  #   woody500m = vapply(attr_out_list, function(x) x[["woody500m"]], FUN.VALUE = 3.5),
  #   woody3000m = vapply(attr_out_list, function(x) x[["woody3000m"]], FUN.VALUE = 3.5),
  #   noisy_miner = vapply(attr_out_list, function(x) x[["noisy_miner"]], FUN.VALUE  = 0),
  #   IsRemnant = vapply(attr_out_list, function(x) x[["IsRemnant"]], FUN.VALUE = 0),
  #   usedlon = vapply(attr_out_list, function(x) {
  #     a <- x[["usedlon"]]
  #     if (is.null(a)){return(NA_real_)}
  #     else{return(a)}
  #   }, FUN.VALUE = 1.1),
  #   usedlat = vapply(attr_out_list, function(x) {
  #     a <- x[["usedlat"]]
  #     if (is.null(a)){return(NA_real_)}
  #     else{return(a)}
  #   }, FUN.VALUE = 1.1)
  # )
  return(outtable)
}

checkpatchattr <- function(specifiedvals){
  checks <- c(
    nm = isTruthy((nchar(specifiedvals$nm) > 0)),
    woodlandtype = isTruthy(nchar(specifiedvals$woodlandtype) > 0),
    woody500m <- isTruthy(specifiedvals$woody500m),
    woody3000m <- isTruthy(specifiedvals$woody500m)
  )
  print(checks)
  return(checks)
}

patchequalsdefault <- function(specifiedvals){
  valscut <- specifiedvals[!unlist(lapply(specifiedvals, is.null))]
  out <- isTRUE(all.equal(unlist(valscut), unlist(defaultpatchvalues)[names(valscut)]))
  return(out)
}