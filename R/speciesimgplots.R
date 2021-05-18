# functions used in predictionsdetails_module for plotting species images

specimageOut <- function(specinfo, id, height = "200px", includestory = TRUE){
	if (grepl("birdlifephotography.org.au", specinfo$copyrightholder)){
		cpyrht <- tags$div(linknewtab(href = "birdlifephotography.org.au",
                                     HTML("&copy;"),
				     gsub("birdlifephotography.org.au", "", specinfo$copyrightholder)))
	} else {
		cpyrht <- tags$div(HTML("&copy;"),
				   specinfo$copyrightholder
				   )
	}

     out <- tags$div(specinfo$species,
		 infotooltip(title = specinfo$story),
                  style = "float: left",
                  tags$br(),
        linknewtab(
          href = specinfo$url,
          style="text-align: center",
          tags$img(src = gsub(".*/data/birdlifeimgs/", "", specinfo$imgfilename),
                   alt = specinfo$species, height = height),
          # imageOutput(id, height = height, inline = TRUE),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          `data-viewport` = "{'selector': ':root'}",
          title = specinfo$story),
	  cpyrht
        )
    return(out)
        }


