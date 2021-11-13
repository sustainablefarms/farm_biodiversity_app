checkcvals <- function(cvals){
  warn_region <- switch(as.character(isTruthy(cvals$selected_region)),
                        "TRUE" = "", "FALSE" = "Please select a region")
  warn_nopatches <- switch(as.character(isTruthy(cvals$patchattr_tbl)),
                           "TRUE" = "", "FALSE" = "Please add a woodland area")
  passessments <- NULL
  if (isTruthy(cvals$patchattr_tbl)){
    df <- as.data.frame(matrix(NA,
                        nrow = nrow(cvals$patchattr_tbl),
                        ncol = 5))
    names(df) <- c("woody500m", "woody3000m", "noisy_miner", "IsRemnant", "pid")
    colsincommon <- sort(intersect(names(cvals$patchattr_tbl), names(df)))
    df[, colsincommon] <- cvals$patchattr_tbl[, colsincommon]
    names(df) <- c("nearby woody cover",
                   "regional woody cover", 
                   "Noisy Miner presence",
                   "type of woodland",
                   "pid"
                   ) #nice names for printing
    passessments <- lapply(1:nrow(df), function(id) assesspatchrow(df[id, ]))
  }
  
  assessments <- c(
    warn_region,
    warn_nopatches,
    passessments
  )
  empty <- unlist(lapply(assessments, function(x) isTruthy(x == "")))
  assessments <- assessments[!empty]
  return(assessments)
}

assesspatchrow <- function(patt){
  checked <- unlist(lapply(patt, function(x) isTruthy(is.finite(x))))
  if (sum(checked == FALSE) > 0){
    return(paste("Woodland area", patt$pid, "needs:", 
                                       paste(names(checked)[!checked], collapse = ", ")))}
  return("")
}
