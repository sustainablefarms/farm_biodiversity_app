# functions used in predictionsdetails_module for plotting species images
specinfocomponents <- function(specinfo, height = "200px"){
  cpyrht <- tags$div(style = "font-size: 70%",
                     linknewtab(href = paste0("https://birdlifephotography.org.au/index.php/show-image?return=search&single&id=", specinfo$birdlife_id),
                                HTML(paste0("&copy;", gsub("birdlifephotography.org.au", "", specinfo$copyrightholder))),
                                class = "cpyrhtlink"))
  
  imgobj <- linknewtab(
    href = specinfo$url,
    class = "imglink",
    style="text-align: center",
    tags$img(src = gsub(".*/data/birdlifeimgs/", "", specinfo$imgfilename),
             id = specinfo$species,
             class = "specimg",
             alt = specinfo$species, height = height))
  return(list(
    cpyrht = cpyrht,
    img = imgobj
  ))
}

