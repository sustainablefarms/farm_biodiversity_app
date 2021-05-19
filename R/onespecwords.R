onespecwords <- function(specname, species_prob_current){
  out <- paste0(
    "According to our model, there is a ",
    format(species_prob_current[specname, "median"] * 100, digits = 2),
    "% (lower bound = ",
    format(species_prob_current[specname, "lower"] * 100, digits = 2),
    "%, upper bound = ",
    format(species_prob_current[specname, "upper"] * 100, digits = 2),
    "%) chance of the ",
    specname,
    " occupying at least one of your woodland patches."
  )
  return(out)
}