rfiles <- list.files("./R", full.names = TRUE)
rootfiles <- c(
  "app.R",
  "DESCRIPTION",
  "NAMESPACE"
)

wwwfiles1 <- list.files("./www", pattern = "^lowres.*", full.names = TRUE) # birdlife images
wwwfiles2 <- paste0("./www/", c(
  "Sustainable Farms logo RGB.png",
  "SFsimple_title.svg",
  "ANU_Secondary_Horizontal_GoldBlack.svg",
  "speciestable.html",
  "google-analytics.html",
  "extra.html",
  "_accordion.scss",
  "base.scss"))
# setdiff(list.files("./www", full.names = TRUE), c(wwwfiles1, wwwfiles2))

datafiles <- paste0("./data/", 
c("nicecovarnames.csv",
"report.Rmd",
"alaimgs_final/c4c37912-34ea-420b-9c77-65b59a8c9391.jpg",
"sa2_points_climate.rds",
"state_borders.rds",
"sa2_polygons.rds",
"sa2_points.rds",
"sa2_polygons_bbox.rds",
"sa2_polygons_bboxtotal.rds",
"species_birdlifeimgs.csv",
"species_shortstory.csv",
"birdinfotable.rds",
"consstatus.rds",
"model_data.rds",
"traits.rds"))

files <- c(rootfiles, datafiles, rfiles, wwwfiles1, wwwfiles2, "./scripts/_functions.scss", "./scripts/_variables.scss")
cat(files, file = "./data/filemanifest.txt", sep = "\n")
