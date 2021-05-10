# ALA image API info and testing: https://images.ala.org.au/ws#/JSON%20services%20for%20accessing%20and%20updating%20metadata/image%2F%7BimageID%7D
# get ALA images
# url <- 
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
# media <- ala_media(traits$`Scientific Name`[[1]],
#                    filters =  select_filters(basisOfRecord = "Image", profile = "ALA"),
#                    columns = select_columns("imageId", "image_url", "all_image_url", "scientificName", group = "basic"),
#                    download_dir = "./data/alaimgs/"
# )
# 
# occ <- ala_occurrences(traits$`Scientific Name`,
#                    filters =  select_filters(basisOfRecord = "Image"),
#                    columns = select_columns("common_name_and_lsid", "raw_common_name",
#                                             "imageId", "imageUrl", "image_url", "all_image_url", "scientificName", group = "basic")
# )

