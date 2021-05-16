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

        tags$div(specinfo$species,
		 infotooltip(title = specinfo$story),
                  style = "float: left",
                  tags$br(),
        linknewtab(
          href = specinfo$url,
          style="text-align: center",
          imageOutput(id, height = "200px", inline = TRUE),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          `data-viewport` = "{'selector': ':root'}",
          title = specinfo$story),
	  cpyrht
        )
        }


