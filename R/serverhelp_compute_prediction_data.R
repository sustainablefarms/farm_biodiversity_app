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

#' @description Get species probabilities for a given Xocc
get_spec_prob <- function(model_data, Xocc){ 
  modwXocc <- msod::supplant_new_data(model_data, Xocc, toXocc = function(x){stdXocc(x, model_data$XoccProcess$center,
                                                                               model_data$XoccProcess$scale,
                                                                               model_data$XoccColNames)})
  species_prob <- msod::poccupancy_margotherspeciespmaxsite.jsodm_lv(modwXocc)
  return(species_prob)
}

#' @description Make alternative Xoccs - for use in richness comparisons, and other summaries
alternative_Xoccs <- function(Xocc){#low and high WCF only for now
  Xoccs <- list(lowWCF500 = Xocc,
                highWCF500 = Xocc)
  Xoccs$lowWCF500$WCF_500 <- 2
  Xoccs$highWCF500$WCF_500 <- 20
  return(Xoccs)
}

#' @description Returns nicer names for the different Xocc alternatives, and a fixed ordering
alternative_Xoccs_nicename <- function(techname){
  nicenames <- 
    c(current = "Your estimate",
      reference = "Reference estimate",
      lowWCF500 = "Less woody canopy nearby",
      highWCF500 = "More woody canopy nearby")
  nameorder <- 
    c(current = 2,
      reference = 4,
      lowWCF500 = 1,
      highWCF500 = 3)
  # return a factor of the nice names, with the ordering of the factor determined by nameorder
  out <- factor(nicenames[techname],
         levels = nicenames[names(sort(nameorder[techname], decreasing = TRUE))], 
         ordered = TRUE)
  return(out)
}

#' @description Given a list of species probabilities for different alternatives, 
#' compute the expected number of species for each alternative, 
#' and return it in a nice data frame with an ordered factor for names.
specrich_all <- function(spec_probs){
  species_richness_raw <- vapply(spec_probs,
                               function(x){sum(x[, "median"])}, FUN.VALUE = 1.11)
  warning("Richness ignores interactions between species. Interactions were too time consuming to include.")
  out <- data.frame(E = species_richness_raw,
                    category = alternative_Xoccs_nicename(names(species_richness_raw)))
  return(out)
}

#' @description Richness of v. small birds (threshold of 20g from Ikin et al 2019, Table 1)
specrich_vsmall <- function(spec_probs){
  vsmallspecnames <- traits$`Common Name`[traits$`Body Mass` < 20]
  species_richness_raw <- vapply(spec_probs,
                               function(x){sum(x[vsmallspecnames, "median"])}, FUN.VALUE = 1.11)
  warning("Richness ignores interactions between species. Interactions were too time consuming to include.")
  out <- data.frame(E = species_richness_raw,
                    category = alternative_Xoccs_nicename(names(species_richness_raw)))
  return(out)
}

