# delete Australian Wood Duck and Australasian Pipit
model_data <- readRDS("./data/model_data.rds")
# save occ to check later
pocc1 <- msod:::get_occ_b(model_data, usesummary = 1)

# species index to remove
idxtoremove <- which(model_data$species %in% c("Australian Wood Duck", "Australasian Pipit"))
nameidx <- 1:length(model_data$species)
names(nameidx) <- model_data$species
nameidx[-idxtoremove] <- 1:(length(model_data$species) - length(idxtoremove))
nameidx[idxtoremove] <- NA

# change mcmc occ.b and det.b, lv.b
mcmc <- model_data$mcmc[[1]]
mcmc <- mcmc[, !grepl(paste0("^(occ|det|lv).b\\[(", paste0(idxtoremove, collapse = "|"), "),.*"), colnames(mcmc))]
newname <- function(oldname){
  if (!grepl("^(det.b|occ.b|lv).*", oldname)){
    return(oldname)
  }
  spltname <- strsplit(oldname, "(\\[|,)")
  oldidx <- as.integer(spltname[[1]][[2]])
  newidx <- nameidx[oldidx]
  stopifnot(!is.na(newidx))
  spltname[[1]][[2]] <- as.character(newidx)
  out <- paste0(spltname[[1]][[1]], "[", spltname[[1]][[2]], ",", spltname[[1]][[3]])
  return(out)
}
newnames <- vapply(colnames(mcmc), newname, FUN.VALUE = colnames(mcmc)[[1]], USE.NAMES = FALSE)
colnames(mcmc) <- newnames
model_data$mcmc[[1]] <- mcmc
stopifnot(length(grep("62", colnames(mcmc))) == 0)

# change species
model_data$species <- model_data$species[-idxtoremove]

# change nspecies
model_data$data$nspecies <- length(model_data$species)

pocc2 <- msod:::get_occ_b(model_data, usesummary = 1)

stopifnot(all.equal(pocc1[-idxtoremove, , , drop = FALSE ], pocc2))

saveRDS(model_data, "./data/model_data.rds")

### Code used to build saved fitted object ###
# fit <- readRDS("../Experiments/10_withAlbertData/fittedmodels/10_7_0_best1065_timewind_2lv.rds")
# fit2 <- msod::minimise_fit.jsodm_lv(fit)
# fit2$XoccProcess <- fit$toXocc$params$mainparams
# fit2$XoccColNames <- colnames(fit$data$Xocc)
# saveRDS(fit2, file = "data/model_data.rds")