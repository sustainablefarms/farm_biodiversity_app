tocommon <- function(species_prob_current){ #this function only changes column names and adds species name as a column
  colnam <- colnames(species_prob_current)
  colnames(species_prob_current) <- dplyr::case_when(
    colnam == "median" ~ "value",
    TRUE ~ colnam
  )

  sp_current <- data.frame(species = rownames(species_prob_current), species_prob_current)
  return(sp_current)
}

todifferent <- function(species_prob_current, species_prob_ref){
  colnam <- colnames(species_prob_current)
  colnames(species_prob_current) <- dplyr::case_when(
    colnam == "median" ~ "value",
    TRUE ~ colnam
  )
  colnam <- colnames(species_prob_ref)
  colnames(species_prob_ref) <- dplyr::case_when(
    colnam == "median" ~ "value",
    TRUE ~ colnam
  )
  
  species_prob_current <- data.frame(species = rownames(species_prob_current), value = species_prob_current[, "value"])
  species_prob_ref <- data.frame(species = rownames(species_prob_ref), value = species_prob_ref[, "value"])
  
  sp_diff <- dplyr::inner_join(species_prob_current, species_prob_ref, by = "species", suffix = c(".cur", ".ref"))
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
  richness_data[[1]]$ms <- 0; richness_data[[3]]$ms <- 10
  richness_data[[1]]$woody500m <- 2; richness_data[[3]]$woody500m <- 20
  pbapply::pboptions(type = "none")
  richness_predictions <- lapply(richness_data, function(a){
    set.seed(4444)
    msod:::occspecrichness_avsite.jsodm_lv(model_data, a)
  })
  richness_df <- as.data.frame(do.call(rbind, richness_predictions))
  richness_df$category <- factor(seq_len(3), levels = seq_len(3),
                                 labels = c("Less vegetation", "Your estimate", "More vegetation"))
  
  species_richness <- richness_df
  
  
  return(species_richness)
}