
prep_birdinfotable <- function(){
  model_data <- load_model_data()
  urls <- get_birdlife_url(model_data$species)
  stories <- prep_birdstories() #readRDS("./data/birdstories.rds")
  imgfilenames <- prep_birdimages() #readRDS("./data/birdinfotable.rds")$imgfilename
  specinfoframe <- data.frame(species = model_data$species,
             url = urls[model_data$species],
             story = unlist(stories[model_data$species], recursive = FALSE),
             imgfilename = imgfilenames[model_data$species])
  saveRDS(specinfoframe, "./data/birdinfotable.rds")
  return(specinfoframe)
}

load_birdinfotable <- function(){
  infotable <- readRDS("./data/birdinfotable.rds")
  infotable$imgfilename <- normalizePath(infotable$imgfilename)
  infotable$shortstory <- "To Fill"
  infotable["Galah", "shortstory"] <- "Galahs are more likely to occupy patches with Noisy Miners"
  speciesinfo <<- infotable
  return(speciesinfo)
}