# library(rvest)
#' @param url url of bird profile webpage
birdlife_extractintropara <- function(url){
  xmlofpage <- xml2::read_html(url)
  if (url == "https://birdlife.org.au/bird-profile/Tree-Martin") {
    intropara <- xmlofpage %>%
      rvest::html_nodes(".page-title") %>%
      rvest::html_nodes(".content") %>%
      rvest::html_nodes(".clearfix") %>%
      rvest::html_node(".col-half") %>%
      `[[`(1) %>%
      rvest::html_text(trim = TRUE)
  } else {
    intropara <- xmlofpage %>%
      rvest::html_nodes(".page-title") %>%
      rvest::html_nodes(".content") %>%
      rvest::html_nodes(".clearfix:not(.inner)") %>%
      rvest::html_text(trim = TRUE)
  }
  if (grepl("Western-Gerygone", url)){
  # for Western Gerygone from this document http://birdswa.com.au/CEC/Handouts/Western%20Australian%20Gerygones.pdf
    intropara1 <- 
"Oddly named since it almost reaches the east coast of Australia. Similar to Brown, Large-
billed, Dusky and Mangrove Gerygones but readily distinguished from them by its bold
black and white tail pattern. When it is seen hovering near foliage a neat bird becomes
strikingly attractive because of its fanned tail. ...
There is evidence of some migratory movement 
generally from south to north during winter. ...
[Its song] has been described
as having the qualities of a leaf falling slowly to the ground but not reaching it."

intropara2 <- paste(
  "The Western Gerygone is a small bird with a pale brownish-grey face that merges into a whitish chin and throat, with a narrow, white ring around the eye.",
  "...",
  "Western Gerygones forage mostly in trees and shrubs, pecking at insects and other invertebrates from the outer foliage, or fluttering in the air to snap at insects.")

    intropara <- gsub("\\n", "", intropara2)
    credits <-  "Words from https://www.birdlife.org.au/bird-profile/western-gerygone (accessed 17/05/2021)."
    #"Words from http://birdswa.com.au/CEC/Handouts/Western%20Australian%20Gerygones.pdf (accessed 22/03/2021)."
  } else {
    credits <- paste0("Words from ", url, " (accessed ", format(Sys.time(), "%b %d, %Y"), ").")
  }
  intropara <- paste(intropara, credits)
  return(intropara)
}

