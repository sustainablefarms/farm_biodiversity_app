
prep_birdinfotable <- function(){
  model_data <- load_model_data()
  urls <- get_birdlife_url(model_data$species)
  stories <- lapply(urls, birdlife_extractintropara)
  imgs <- read.csv("./data/species_birdlifeimgs.csv", row.names = 1)
  imgs$filename <- paste0(imgs$filename, ".jpg")
  shortstories <- read.csv("./data/species_shortstory.csv", check.names = FALSE, row.names = 1)
  specinfoframe <- data.frame(species = model_data$species,
             url = urls[model_data$species],
             story = unlist(stories[model_data$species], recursive = FALSE),
             imgfilename = imgs[model_data$species, "filename"],
             birdlife_id = imgs[model_data$species, "birdlife_id"],
             copyrightholder = imgs[model_data$species, "copyrightholder"],
             shortstory = shortstories[model_data$species, 1])
  rownames(specinfoframe) <- specinfoframe$species
  saveRDS(specinfoframe, "./data/birdinfotable.rds")
  return(specinfoframe)
}

# infotable <- readRDS("./data/birdinfotable.rds")
# shortstories <- read.csv("./data/species_shortstory.csv", check.names = FALSE, row.names = 1)
# infotable$shortstory <- shortstories[rownames(infotable), 1]
# saveRDS(infotable, "./data/birdinfotable.rds")

load_birdinfotable <- function(){
  infotable <- readRDS("./data/birdinfotable.rds")
  infotable$imgfilename <- paste0("lowres-", infotable$imgfilename)
  tmp <- gsub('(.{1,20})(\\s|$)', '\\1<br>', infotable$shortstory) #20 characters long and leaves a <br> at the end
  infotable$shortstory <- gsub('<br>$', '', tmp)
  speciesinfo <<- infotable
  return(speciesinfo)
}

# get a list of the photographers
# gsub(" 2.*", "", imgs$copyrightholder) %>%
# unique() %>%
# paste(collapse = ", ")
