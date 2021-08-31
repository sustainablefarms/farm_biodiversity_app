get_birdlife_url <- function(commonnames){
  urls <- paste0("https://birdlife.org.au/bird-profile/", gsub(" ", "-", commonnames))
  names(urls) <- commonnames
  return(urls)
}
