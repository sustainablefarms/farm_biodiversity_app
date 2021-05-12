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

#### Search through image files ####
#' mediaidmap <- read.csv("./data/species_alaimages.csv", check.names = FALSE)[, -1]
#' specidx <- 21
# media <- galah::ala_media(mediaidmap$`Scientific Name`[[specidx]],
#                    filters =  galah::select_filters(basisOfRecord = "Image",
#                                                     profile = "ALA"),
#                    columns = galah::select_columns("creator", "imageId", "image_url", "all_image_url", "scientificName", group = "basic"),
#                    download_dir = "./data/alaimgs/"
# )
# idtokeep <- media$media_id[grepl("Attribution", media$license) & vapply(nchar(media$creator) > 0, isTRUE, FUN.VALUE = FALSE)]
# # remove files without license
# file.remove(paste0("./data/alaimgs/", media$media_id[!(media$media_id %in% idtokeep)], ".jpg"))
# 
# #view images in R
# imageidx <- 1
# plot_jpeg(paste0("./data/alaimgs/", idtokeep, ".jpg")[[imageidx]])
# mediaidmap[specidx, "media_id"] <- idtokeep[[imageidx]]
# 
# write.csv(mediaidmap, file = "./data/species_alaimages.csv", row.names = FALSE)


plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}

#### Move saved images ####
# file.rename(paste0("./data/alaimgs/",mediaidmap$media_id, ".jpg"), paste0("./data/alaimgs_final/", mediaidmap$media_id, ".jpg"))





#### OTHER ####
# occ <- galah::ala_occurrences(traits$`Scientific Name`[[1]],
#                    filters =  galah::select_filters(basisOfRecord = "Image",
#                                              profile = "ALA"),
#                    columns = galah::select_columns("licence", "creator", "common_name_and_lsid", "raw_common_name",
#                                             "imageId", "imageUrl", "image_url", "all_image_url", "scientificName", group = "basic")
# )

