
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
             copyrightholder = imgs[model_data$species, "copyrightholder"],
             shortstory = shortstories[model_data$species, 1])
  rownames(specinfoframe) <- specinfoframe$species
  saveRDS(specinfoframe, "./data/birdinfotable.rds")
  return(specinfoframe)
}

load_birdinfotable <- function(){
  infotable <- readRDS("./data/birdinfotable.rds")
  speciesinfo <<- infotable
  return(speciesinfo)
}
