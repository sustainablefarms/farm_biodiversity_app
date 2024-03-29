onespecwords <- function(specname, species_prob_current, refpredictions, refisaverage = FALSE){
  relativeimprovement <- (species_prob_current[specname, "median"]/refpredictions[specname, "median"]) - 1
  linimprovement <- (species_prob_current[specname, "median"] - refpredictions[specname, "median"])
  improvement <- linimprovement
  out <- paste(
    paste(appname, "estimates that there is a",
        sprintf("%0.*f%%",
                switch(as.character(species_prob_current[specname, "median"] > 0.01), "TRUE" = 0, "FALSE" = 2),
                species_prob_current[specname, "median"] * 100),
        "chance of the",
        specname,
        "occupying at least one of the woodland areas in",
        if (refisaverage){"Scenario 1"}else{"Scenario 2"},
        sprintf("(lower bound = %0.2f%%, upper bound = %0.2f%%).",
                species_prob_current[specname, "lower"] * 100,
                species_prob_current[specname, "upper"] * 100
                )
	),
    paste(
      sprintf("This is a %0.*f%% %s in occupancy probability",
              switch(as.character(abs(improvement) > 0.01), "TRUE" = 0, "FALSE" = 2),
              abs(improvement) * 100,
              switch(as.character(improvement < 0),
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
  # change any use of a 8.. or a 11 to an 8 and an 11
  out <- gsub("a 8", "an 8", out)
  out <- gsub("a 11%", "an 11%", out)
  out <- gsub("a 18%", "an 18%", out)
  return(out)
}
