# functions used in predictionsdetails_module for plotting species images

specimageOut <- function(specinfo, id, height = "200px", includestory = TRUE){
        # column(2, 
        tags$div(specinfo$species,
		 infotooltip(title = specinfo$species),
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
        tags$div("Creator: = ???"),
	tags$div(linknewtab(href="https://creativecommons.org/licenses/by-nc-sa/3.0/",
                   tags$img(src = "https://licensebuttons.net/l/by-nc-sa/3.0/88x31.png",
                            alt = "CC BY-NC-SA 3.0",
                            height = "20px")
          ))
        )
        # )
        }


