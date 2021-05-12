# ALA image API info and testing: https://images.ala.org.au/ws#/JSON%20services%20for%20accessing%20and%20updating%20metadata/image%2F%7BimageID%7D
# get ALA images
# url <- 
# get credits
#' @examples 
#' imageId <- "fdb52fe5-7bc6-4307-8beb-998ea4b6f3b4"
getalacredits <- function(imageId){
  handle <- curl::new_handle()
  handle <- curl::handle_setheaders(handle, Accept = "Application/json")
  a <- curl::curl_download(paste0("https://images.ala.org.au/ws/image/",
                                  imageId,
                                  "?includeTags=true&includeMetadata=true"), 
                           destfile = paste0("./data/alaimgs/", imageId, ".txt"),
                           handle = handle)
  info <- xml2::as_list(xml2::read_xml(paste0("./data/alaimgs/", imageId, ".txt")))[[1]]
  file.remove(paste0("./data/alaimgs/", imageId, ".txt"))
  keys <- lapply(info, function(x) attr(x, "key", exact = TRUE))
  names(info) <- keys
  licence <- info[["license"]]
  creator <- info[["creator"]]
  link <- paste0("https://images.ala.org.au/image/details?imageId=", imageId)
  return(list(
    licence = licence,
    creator = creator,
    link = link
  ))
}

getalaimages <- function(){
  traits <- loadtraits2global()
  counts <- lapply(traits$`Scientific Name`,
                   function(sn){
                     counts <- galah::ala_counts(sn,
                                                 filters =  galah::select_filters(basisOfRecord = "Image", profile = "ALA"))
                   })
  names(counts) <- traits$`Common Name`
  counts <- unlist(counts)
  stopifnot(all(counts > 0))
  
  
}


# counts <- ala_counts(traits$`Scientific Name`,
#    select_filters(basisOfRecord = "Image")
# )
# 
# media <- ala_media(traits$`Scientific Name`,
#                    filters =  select_filters(basisOfRecord = "Image", profile = "ALA"),
#                    columns = select_columns("imageID", "image_url", "all_image_url", "scientificName", group = "basic"),
#                    download_dir = "./data/alaimgs/"
# )

