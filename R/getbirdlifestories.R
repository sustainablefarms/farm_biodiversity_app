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
    credits <- paste("Words from http://birdswa.com.au/CEC/Handouts/Western%20Australian%20Gerygones.pdf (accessed 22/03/2021).",
                     paste0("Image from ", url, " (accessed ", format(Sys.time(), "%b %d, %Y"), ")."))
  } else {
    credits <- paste0("Image and words from ", url, " (accessed ", format(Sys.time(), "%b %d, %Y"), ").")
  }
  intropara <- paste(intropara, credits)
  return(intropara)
}

birdlife_getdescription <- function(commonnames){
  urls <- get_birdlife_url(commonnames)
  introparas <- lapply(urls, birdlife_extractintropara)
  return(introparas)
}

prep_birdstories <- function(){
  model_data <- load_model_data()
  stories <- birdlife_getdescription(model_data$species)
  
  # for Western Gerygone from this document http://birdswa.com.au/CEC/Handouts/Western%20Australian%20Gerygones.pdf
  westerngerygoneblurb <- 
"Oddly named since it almost reaches the east coast of Australia. Similar to Brown, Large-
billed, Dusky and Mangrove Gerygones but readily distinguished from them by its bold
black and white tail pattern. When it is seen hovering near foliage a neat bird becomes
strikingly attractive because of its fanned tail. ...
There is evidence of some migratory movement 
generally from south to north during winter. ...
[Its song] has been described
as having the qualities of a leaf falling slowly to the ground but not reaching it."
  
  
  stories[["Western Gerygone"]] <- gsub("\\n", "", westerngerygoneblurb)
  saveRDS(stories, file = "./data/birdstories.rds")
  return(stories)
}

