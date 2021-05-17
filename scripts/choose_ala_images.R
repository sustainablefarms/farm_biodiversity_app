plot_jpeg = function(path, add=FALSE)
{
  require('jpeg')
  jpg = readJPEG(path, native=T) # read the file
  res = dim(jpg)[2:1] # get the resolution, [x, y]
  if (!add) # initialize an empty plot area if add==FALSE
    plot(1,1,xlim=c(1,res[1]),ylim=c(1,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(jpg,1,1,res[1],res[2])
}

#### Search through image files ####
mediaidmap <- read.csv("./data/species_alaimages.csv", check.names = FALSE)
specidx <- 30
mediaidmap[specidx, ]
media <- galah::ala_media(mediaidmap[specidx, "Scientific Name"],
                   filters =  galah::select_filters(basisOfRecord = "Image",
                                                    profile = "ALA"),
                   columns = galah::select_columns("creator", "imageId", "image_url", "all_image_url", "scientificName", group = "basic"),
                   download_dir = "./data/alaimgs/"
)
idtokeep <- media$media_id[grepl("Attribution", media$license) & vapply(nchar(media$creator) > 0, isTRUE, FUN.VALUE = FALSE)]
# remove files without license
file.remove(paste0("./data/alaimgs/", media$media_id[!(media$media_id %in% idtokeep)], ".jpg"))

#view images in R
imageidx <- 1
plot_jpeg(paste0("./data/alaimgs/", idtokeep, ".jpg")[[imageidx]])
mediaidmap[specidx, "media_id"] <- idtokeep[[imageidx]]

write.csv(mediaidmap, file = "./data/species_alaimages.csv", row.names = FALSE)

#### Move saved images ####
file.rename(paste0("./data/alaimgs/",mediaidmap$media_id, ".jpg"), paste0("./data/alaimgs_final/", mediaidmap$media_id, ".jpg"))





#### OTHER INFO DOWNLOAD ####
# occ <- galah::ala_occurrences(traits$`Scientific Name`[[1]],
#                    filters =  galah::select_filters(basisOfRecord = "Image",
#                                              profile = "ALA"),
#                    columns = galah::select_columns("licence", "creator", "common_name_and_lsid", "raw_common_name",
#                                             "imageId", "imageUrl", "image_url", "all_image_url", "scientificName", group = "basic")
# )
