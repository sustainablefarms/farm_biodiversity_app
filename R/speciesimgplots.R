# functions used in predictionsdetails_module for plotting species images

specimageOut <- function(specinfo, height = "200px"){
		cpyrht <- tags$div(style = "font-size: 70%", linknewtab(href = "birdlifephotography.org.au",
                                     HTML(paste0("&copy;",
				     gsub("birdlifephotography.org.au", "", specinfo$copyrightholder)))))

     out <- tags$div(specinfo$species,
		 infotooltip(title = specinfo$story),
                  style = "float: left; text-align: center",
                  tags$br(),
        linknewtab(
          href = specinfo$url,
          style="text-align: center",
          tags$img(src = gsub(".*/data/birdlifeimgs/", "", specinfo$imgfilename),
                   alt = specinfo$species, height = height),
          `data-toggle` = "tooltip",
          `data-placement` = 'auto top',
          `data-viewport` = "{'selector': ':root'}",
          title = specinfo$story),
	  cpyrht
        )
    return(out)
        }


