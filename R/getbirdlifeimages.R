get_birdlife_url <- function(commonnames){
  urls <- paste0("https://birdlife.org.au/bird-profile/", gsub(" ", "-", commonnames))
  names(urls) <- commonnames
  return(urls)
}

birdlife_getimages <- function(commonnames){
  urls <- get_birdlife_url(commonnames)
  imgurls <- lapply(urls, function(url){
    xmlofpage <- xml2::read_html(url)
    imgpath <- xmlofpage %>%
      rvest::html_nodes(".page-title") %>%
      rvest::html_nodes("img") %>%
      `[[`(1) %>%
      rvest::html_attr("src")
    if (grepl("Tree.Martin")){ # the second image is better for Tree Martins
      imgpath <- xmlofpage %>%
        rvest::html_nodes(".page-title") %>%
        rvest::html_nodes("img") %>%
        `[[`(2) %>%
        rvest::html_attr("src")
    }
    imgurl <- paste0("https://birdlife.org.au", imgpath)
    return(imgurl)
  })
  dir.create("./data/birdstories/", showWarnings = FALSE)
  destfiles <- paste0("./data/birdstories/", gsub(" ", "-", names(imgurls)), ".", gsub(".*\\.", "", imgurls))
  names(destfiles) <- names(imgurls)
  mapply(download.file, url = imgurls, destfile = destfiles)
  return(destfiles)
}

prep_birdimages <- function(){
  model_data <- load_model_data()
  imgfilenames <- birdlife_getimages(model_data$species)
  
  saveRDS(imgfilenames, file = "./data/imgfilenames.rds")
  return(imgfilenames)
}
