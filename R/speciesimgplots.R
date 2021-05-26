# functions used in predictionsdetails_module for plotting species images

specimageOut <- function(specinfo, height = "200px"){
	 cpyrht <- tags$div(style = "font-size: 70%",
	                    linknewtab(href = paste0("https://birdlifephotography.org.au/index.php/show-image?return=search&single&id=", specinfo$birdlife_id),
                                     HTML(paste0("&copy;", gsub("birdlifephotography.org.au", "", specinfo$copyrightholder)))))

     out <- tags$div(specinfo$species,
		 infotooltip(title = specinfo$story),
                  style = "float: left; margin: 5px",
                  tags$br(),
        linknewtab(
          href = specinfo$url,
          style="text-align: center",
          tags$img(`data-src` = gsub(".*/data/birdlifeimgs/", "", specinfo$imgfilename),
		   src = "",
		   id = specinfo$species,
		   class = "specimg",
                   alt = specinfo$species, height = height),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
#          `data-viewport` = "{'selector': ':root'}",
          title = specinfo$story),
	  cpyrht
        )
    return(out)
        }


