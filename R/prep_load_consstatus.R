# load constatus
load_constatus <- function(){
  consstatus <- readRDS("./data/consstatus.rds")
  consstatus <- consstatus[order(consstatus$CommonName), ] #sort alphabetically
  consstatus["Superb Parrot", "statussummary"] <- "is listed as vulnerable federally and in Victoria, and is listed as threatened in NSW."
  consstatus["Dusky Woodswallow", "statussummary"] <- "is listed as vulnerable in NSW."
  consstatus["Brown Treecreeper", "statussummary"] <- "is listed as vulnerable in NSW."
  consstatus["Grey-crowned Babbler", "statussummary"] <- "is listed as vulnerable in NSW and threatened in Victoria."
  consstatus["Diamond Firetail", "statussummary"] <- "is listed as vulnerable in NSW and threatened in Victoria."
  return(consstatus)
}