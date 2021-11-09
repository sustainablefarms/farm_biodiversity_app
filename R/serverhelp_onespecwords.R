onespecwords <- function(specname, species_prob_current, refpredictions, refisaverage = FALSE){
  relativeimprovement <- (species_prob_current[specname, "median"]/refpredictions[specname, "median"]) - 1
  out <- paste(
    paste(appname, "estimates that there is a",
        sprintf("%0.*f%% (lower bound = %0.2f%%, upper bound = %0.2f%%)",
                switch(as.character(species_prob_current[specname, "median"] > 0.01), "TRUE" = 0, "FALSE" = 2),
                species_prob_current[specname, "median"] * 100,
                species_prob_current[specname, "lower"] * 100,
                species_prob_current[specname, "upper"] * 100
                ),
        "chance of the",
        specname,
        "occupying at least one of your woodland patches in",
        if (refisaverage){"Scenario 1."}else{"Scenario 2."}),
    paste(
      sprintf("This is a %0.*f%% %s in occupancy probability",
              switch(as.character(abs(relativeimprovement) > 0.01), "TRUE" = 0, "FALSE" = 2),
              abs(relativeimprovement) * 100,
              switch(as.character(relativeimprovement < 0),
                     "TRUE" = "reduction",
                     "FALSE" = "increase")),
      "compared to the estimate for",
      if (refisaverage){"an average woodland area,"}else{"Scenario 1,"},
      "which was",
      sprintf("%.*f%%.", 
              switch(as.character(refpredictions[specname, "median"] > 0.01), "TRUE" = 0, "FALSE" = 2),
              refpredictions[specname, "median"] * 100)
    )
  )
  return(out)
}