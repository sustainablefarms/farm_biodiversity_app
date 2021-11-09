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
  print(outtable)
  return(outtable)
}

# jagged <- list(list(woody500m = 9.5, woody3000m = 9, usedlon = NULL, pid = 1))
# jagged <- list(list(woody500m = 9.5, woody3000m = 9, usedlon = NULL, pid = 1),
#                list(NULL))

jagged_2_df <- function(attr_out_list){
  outtable <- as.data.frame(dplyr::bind_rows(attr_out_list))
  keep <- vapply(outtable$woody3000m, isTruthy, FUN.VALUE = FALSE)  #all empty (non-UI-existing patches) have NA or similar values for woody3000m
  if (sum(keep) == 0){return(NULL)}
  outtable <- outtable[keep, , drop = FALSE]
  return(outtable)
}

patchequalsdefault <- function(specifiedvals){
  if (!isTruthy(specifiedvals)){return(FALSE)}
  valscut <- specifiedvals[!unlist(lapply(specifiedvals, is.null))]
  out <- isTRUE(all.equal(unlist(valscut), unlist(defaultpatchvalues)[names(valscut)]))
  return(out)
}

getinusepid <- function(attr_table){
  pidsinuse <- if (isTruthy(attr_table)){attr_table$pid} else {c()}
  return(pidsinuse)
}

insertblankwoodlandarea <- function(attr_table, ns, maxpatchnum, session = getDefaultReactiveDomain()){
    pidsinuse <- getinusepid(attr_table)
    newid <- min(setdiff(1:maxpatchnum, pidsinuse))
    pattr <- defaultpatchvalues
    # if (is.data.frame(newinattr())){ for non-blank woodland areas - used elsewhere
    #   rowintable <- which(newinattr()$pid == newid)
    #   if (isTruthy(rowintable)){pattr <- newinattr()[rowintable[[1]], ]}
    # }
    # insertUI
   newUI <- patchattr_UI(ns(paste0("p", newid)), newid, pattr)
   insertUI(paste0("#", ns("placeholder")),
             where = "beforeBegin",
             ui = newUI
             )
   invisible(newUI)
}