attrlist2attrtbl <- function(attr_out_list){
  # attr_out_list <- lapply(patchidsinuse(), function(pid){
  #   if (!(pid %in% patchidsinuse())){return(NULL)}
  #   attr_pid <- attr_out_r[[pid]]()
  #   # when patch is initialised it has NULL attributes briefly, hence the following
  #   empty <- sum(unlist(lapply(attr_pid, function(x) !is.null(x))), na.rm = TRUE) == 0
  #   if (isTruthy(empty)){return(NULL)}
  #   attr_pid$pid <- pid
  #   return(attr_pid)
  # })
  # # keep only the non-null values, often at start of app attr_out_list <- list(NULL)
  # attr_out_list <- attr_out_list[!vapply(attr_out_list, is.null, FUN.VALUE = TRUE)]
  # validate(need(length(attr_out_list) > 0, ""))
  
  outtable <- jagged_2_df(attr_out_list)
  return(outtable)
}

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