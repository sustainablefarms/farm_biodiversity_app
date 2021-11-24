tocommon <- function(spec_prob){ #this function only changes column names and adds species name as a column
  colnam <- colnames(spec_prob)
  colnames(spec_prob) <- dplyr::case_when(
    colnam == "median" ~ "value",
    TRUE ~ colnam
  )

  sp_current <- data.frame(species = rownames(spec_prob), spec_prob)
  return(sp_current)
}

todifferent <- function(spec_prob, species_prob_ref){
  colnam <- colnames(spec_prob)
  colnames(spec_prob) <- dplyr::case_when(
    colnam == "median" ~ "value",
    TRUE ~ colnam
  )
  colnam <- colnames(species_prob_ref)
  colnames(species_prob_ref) <- dplyr::case_when(
    colnam == "median" ~ "value",
    TRUE ~ colnam
  )
  
  spec_prob <- data.frame(species = rownames(spec_prob), value = spec_prob[, "value"])
  species_prob_ref <- data.frame(species = rownames(species_prob_ref), value = species_prob_ref[, "value"])
  
  sp_diff <- dplyr::inner_join(spec_prob, species_prob_ref, by = "species", suffix = c(".cur", ".ref"))
  sp_diff$value <- sp_diff$value.cur / sp_diff$value.ref
  return(sp_diff)
}

#' @param df A matrix or data frame
#' @param n The number of rows to extract, ordered by `rankingcolumn`
#' @param rankingcolumn The values in `rankingcolumn` are used to order the rows.
#' @description Returns the top `n` rows according to `rankingcolumn`
topnrows <- function(df, n, rankingcolumn){
  return(df[order(df[, rankingcolumn], decreasing = TRUE)[1:n], ])
}

compute_richness <- function(model_data, Xocc){
  # richness calculations
  # get richness
  richness_data <- list(low = Xocc, current = Xocc, high = Xocc)
  # richness_data[[1]]$NMdetected <- 0; richness_data[[3]]$NMdetected <- 1
  richness_data[[1]]$WCF_500 <- 2; richness_data[[3]]$WCF_500 <- 20
  pbapply::pboptions(type = "none")
  richness_predictions <- lapply(richness_data, function(a){
    mod <- msod::supplant_new_data(model_data, a, 
                                            toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                            model_data$XoccProcess$scale,
                                                                            model_data$XoccColNames)})
    specpred <- msod:::poccupancy_mostfavourablesite.jsodm_lv(mod)
    return(E = sum(specpred[ , "median"]))
  })
  warning("Computations ignore interactions between species - faster and expectations may ignore these anyway")
  richness_df <- as.data.frame(do.call(rbind, richness_predictions))
  names(richness_df)[[1]] <- "E"
  
  species_richness <- richness_df
  
  
  return(species_richness)
}